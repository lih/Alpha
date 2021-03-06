{-# LANGUAGE RankNTypes, ParallelListComp, TupleSections #-}
module Specialize(specialize) where

import Context.Types
import Control.Arrow
import Control.Monad.Trans.Reader
import Data.Array
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Word
import ID
import My.Control.Monad
import My.Control.Monad.State
import My.Control.Monad.RWTL
import My.Data.Either
import My.Data.List
import My.Data.Tree
import PCode
import Specialize.Architecture
import Specialize.Architecture
import Specialize.Types
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified My.Data.Relation as R
import qualified Data.Set as S

import System.IO.Unsafe
import My.Prelude

worldID = ID (-1)

specialize arch env (Code args code retVar) = (sum sizes,B.concat $< sequence codes)
  where
    ~(estimates,sizes,codes) = unzip3 [v | BC v <- elems instructions]
    (past,future) = archInitials arch args retVar
    (bounds,instr,nexts,prevs) = navigate code
    defSize = archDefaultSize arch
    positions = listArray bounds posList
    posList = [(e,s) | e <- sums estimates | s <- sums sizes]
    codeTree = spanningTree 0 nexts
    runInstr i p f past = runRWTL (compile (instr i) >> align) ((infos!i) (i,getPos)) p f
      where compile = archCompileInstr arch
            getPos j = (e,d,past j) where ~(e,d) = positions!j
            align | not (isBranch (instr i)) && length (prevs $ head $ nexts i) > 1 = compile Noop
                  | otherwise = doNothing

    treeArray next seed = array bounds $ flatten $ descend (\i e -> ((i,e),next i e (instr i))) seed codeTree

    instructions = fmap snd instructions'
    instructions' = instrs
      where
        getsi f = gets $ \a i -> f (a!i)
        puti i e = modify $ (//[(i,e)])
        nextFuture i f = snd4 $ runInstr i (error "undefined past") f (const Nothing)
        gens = array bounds $ zip (flatten codeTree) [0..]
        gens' = array bounds $ zip [0..] (flatten codeTree)
        getPast g i | (case filter (<=(gens!i)) [gens!j | j <- prevs i] of [] -> False ; l -> g==maximum l) = Nothing
                    | otherwise = Just $ fst $ instrs!i
        instrs = array bounds $ flatten $ descend desc past codeTree
          where desc i p = ((i,(p,c)),p')
                  where ~(p',_,_,c) = runInstr i p (snd $ futures!g!i) (getPast g)
                        g = gens!i
        futures = fmap snd $ listArray bounds $ iterate nextFut (1,initial)
          where initial = execState (sequence_ [changeFuture i 0 (futureOf i) | i <- map last (branches codeTree)])
                          (constA bounds (-1,undefined))
                  where futureOf i | null (nexts i) = future
                                   | otherwise = emptyFuture
                nextFut (g,fa) = (g+1,fa')
                  where fa' = execState (sequence_ [changeFuture i g newFut | i <- prevs instr, head (nexts i)==instr]) fa
                        instr = gens'!g ; newFut = Future $ locations $ fst (instrs!instr)
                changeFuture i g f = puti i (g,f) >> mapM_ propagate (prevs i)
                propagate i = do 
                  let j = head (nexts i)
                  gen <- getsi fst ; fut <- getsi snd
                  when (gen i < gen j) $ changeFuture i (gen j) (nextFuture j (fut j))
    
    infos = constA bounds (Info env) `applyA` bindingsA `applyA` sizesA `applyA` activesA `applyA` clobbersA
      where root i v = fmap fst $ M.lookup v (bindingsA!i)
            bindingsA = treeArray next M.empty
              where next _ bnd (Bind bv (Just id)) = insertMany bnd [(s,(id,n)) | (s,n,_) <- flattenBind defSize bv]
                    next _ bnd _ = bnd
            sizesA = treeArray next (M.fromList [(s,n) | bv <- maybe id (:) retVar args, (s,_,n) <- flattenBind defSize bv])
              where next _ bnd (Bind bv _) = insertMany bnd [(s,n) | (s,_,n) <- flattenBind defSize bv]
                    next _ bnd _ = bnd
            activesA = fmap snd $ saturate fun prevs nexts init start
              where init = array bounds [(i,initActives i) | i <- uncurry enumFromTo bounds]
                      where retActives = S.unions [clobbers 0 s | s <- maybe [] bindSyms retVar]
                            initActives i = pair (if isRet (instr i) then retActives else mempty)
                            pair a = (a,a)
                    start = concat [prevs i | i <- indices init, isRet (instr i)]
                    fun i a = (addActives (instr i) out,out)
                      where out = S.unions (map ((a!) >>> fst) (nexts i))
                            addActives (Op _ v vs) s = (s S.\\ clobbers i v)
                                                       <> S.fromList [s | SymVal Value s <- vs]
                                                       <> S.fromList (catMaybes [root i s | SymVal t s <- SymVal Value v:vs
                                                                                          , t==Value || t==Address])
                            addActives (Branch (SymVal Value id) _) s = S.insert id s
                            addActives (Bind bv v) s = maybe id S.insert v $ s S.\\ S.fromList (bindSyms bv)
                            addActives _ s = s
            clobbers i v = ifEmpty (S.singleton v) $ R.lookupRan v (clobbersA!i)
            clobbersA = treeArray next (insertManyA (R.singleton worldID worldID)
                                        [(bindSym bv,s) | bv <- maybe id (:) retVar args
                                                        , bv <- bindNodes bv
                                                        , s <- bindSyms bv])
              where next i r (Bind bv v) = insertManyA r assocs
                      where assocs = [(bindSym bv,s) | bv <- bindNodes bv, s <- bindSyms bv]
                                     ++[(s,s') | v <- maybeToList v
                                               , ref <- S.toList $ references i v
                                               , s <- S.toList $ clobbers i ref
                                               , s' <- bindSyms bv]
                    next i r (Op BCall d (_:args)) = insertManyA r assocs
                      where assocs = [a | ref <- S.toList (argsRefs i args)
                                        , v <- S.toList $ clobbers i ref
                                        , a <- [(v,worldID),(v,v)]]
                    next _ r _ = r
                    insertManyA r as = insertManyR r [a | (x,y) <- as, a <- [(x,y),(y,x)]]
            lookupRefs v r = ifEmpty (S.singleton worldID) $ R.lookupRan v r
            argsRefs i vs = S.fromList [s | SymVal Address s <- vs]
                           <> S.unions [references i s | SymVal Value s <- vs]
            references i v = lookupRefs v (referencesA!i)
            referencesA = treeArray next $ R.setDom worldID (S.fromList $ concatMap bindSyms args) R.empty
              where next i r (Op b v vs) = R.setRan v (argsRefs i vs <> world) r
                      where world = if b==BCall then S.singleton worldID else S.empty
                    next _ r _ = r

ifEmpty def s | S.null s = def
              | otherwise = s

constA bs v = accumArray const v bs []
zipWithA f a b = array (bounds a) [(i,f x y) | (i,x) <- assocs a | y <- elems b]
applyA = zipWithA ($)
insertManyR = foldl (\r (a,b) -> R.insert a b r)
insertMany = foldl (\m (k,v) -> M.insert k v m)

saturate fun nexts prevs init start = f init (array (bounds init) [(i,length $ prevs i) | i <- indices init]) start
  where f a d [] = a
        f a d (i:t) | newElt == a!i = f a d t
                    | otherwise = f (a//[(i,newElt)]) d'' (foldr (insertBy (comparing (d''!))) (filter (/=i) t) (nexts i))
          where newElt = fun i a
                d' = d // [(i,length $ (prevs`asTypeOf`nexts) i)]
                d'' = d' // [(n,(d'!n)-1) | n <- nexts i]
