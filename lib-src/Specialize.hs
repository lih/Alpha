{-# LANGUAGE RankNTypes, ParallelListComp, TupleSections, ImplicitParams #-}
module Specialize(specialize) where

import Control.Arrow
import Data.Array
import Data.Maybe
import Data.Monoid
import Data.Ord
import ID
import My.Control.Monad
import Control.Monad.State.View
import My.Data.List 
import My.Data.Tree
import PCode
import Specialize.Architecture
import Specialize.BinCode
import Specialize.Monad
import Specialize.Memory
import Specialize.Info
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified My.Data.Relation as R
import qualified Data.Set as S

import Misc(debug)

worldID = ID (-1)

specialize arch env (Code args code retVar) = debug code `seq` (sum sizes,B.concat <$> sequence codes)
  where
    ~(estimates,sizes,codes) = unzip3 [v | BC v <- elems instructions]
    (initialPast,initialFuture) = archInitials arch args retVar
    (_bounds,instrAt,nexts,prevs) = navigate code
    defSize = archDefaultSize arch
    positions = listArray _bounds posList
    posList = [(e,s) | e <- sums estimates | s <- sums sizes]
    codeTree = spanningTree 0 nexts
    runInstr i p f past = runSpecialize (compile (instrAt i) >> align) p f
      where compile = let ?info = (infos!i) (i,getPos) in archCompileInstr arch
            getPos j = (e,d,past j) where ~(e,d) = positions!j
            align | not (isBranch (instrAt i)) && length (prevs $ head $ nexts i) > 1 = compile Noop
                  | otherwise = doNothing

    treeArray next seed = array _bounds $ flatten $ descend (\i e -> ((i,e),next i e (instrAt i))) seed codeTree

    instructions = fmap snd instructions'
    instructions' = instrs
      where
        getsi f = gets $ \a i -> f (a!i)
        puti i e = modify $ (//[(i,e)])
        nextFuture i f = f'
          where (_,_,f',_) = runInstr i (error "undefined past") f (const Nothing)
        gens = array _bounds $ zip (flatten codeTree) [0..]
        gens' = array _bounds $ zip [0..] (flatten codeTree)
        getPast g i | (case filter (<=(gens!i)) [gens!j | j <- prevs i] of [] -> False ; l -> g==maximum l) = Nothing
                    | otherwise = Just $ fst $ instrs!i
        instrs = array _bounds $ flatten $ descend desc initialPast codeTree
          where desc i p = ((i,(p,c)),p')
                  where ~(_,p',_,c) = runInstr i p (snd $ futures!g!i) (getPast g)
                        g = gens!i
        futures = fmap snd $ listArray _bounds $ iterate nextFut (1,initial)
          where initial = execState (sequence_ [changeFuture i 0 (futureOf i) | i <- map last (branches codeTree)])
                          (constA _bounds (-1,undefined))
                  where futureOf i | null (nexts i) = initialFuture
                                   | otherwise = emptyFutureState
                nextFut (g,fa) = (g+1,fa')
                  where fa' = execState (sequence_ [changeFuture i g newFut | i <- prevs instr, head (nexts i)==instr]) fa
                        instr = gens'!g ; newFut = FutureState $ locations $ fst (instrs!instr)
                changeFuture i g f = puti i (g,f) >> mapM_ propagate (prevs i)
                propagate i = do 
                  let j = head (nexts i)
                  gen <- getsi fst ; fut <- getsi snd
                  when (gen i < gen j) $ changeFuture i (gen j) (nextFuture j (fut j))
    
    infos = constA _bounds (Info env) `applyA` bindingsA `applyA` sizesA `applyA` activesA `applyA` clobbersA
      where root i v = fmap fst $ M.lookup v (bindingsA!i)
            bindingsA = treeArray next M.empty
              where next _ bnd (Bind bv (Just sym)) = insertMany bnd [(s,(sym,n)) | (s,n,_) <- flattenBind defSize bv]
                    next _ bnd _ = bnd
            sizesA = treeArray next (M.fromList [(s,n) | bv <- maybe id (:) retVar args, (s,_,n) <- flattenBind defSize bv])
              where next _ bnd (Bind bv _) = insertMany bnd [(s,n) | (s,_,n) <- flattenBind defSize bv]
                    next _ bnd _ = bnd
            activesA = saturate fun prevs nexts _init start
              where _init = array _bounds [(i,initActives i) | i <- uncurry enumFromTo _bounds]
                      where retActives = S.unions [clobbers 0 s | s <- maybe [] bindSyms retVar]
                            initActives i = pair (if isRet (instrAt i) then retActives else mempty)
                            pair a = (a,a)
                    start = concat [prevs i | i <- indices _init, isRet (instrAt i)]
                    fun i a = (addActives (instrAt i) out,out)
                      where out = S.unions (map ((a!) >>> fst) (nexts i))
                            addActives (Op _ v vs) act = (act S.\\ clobbers i v)
                                                         <> S.fromList [s | SymVal Value s <- vs]
                                                         <> S.fromList (catMaybes [root i s | SymVal t s <- SymVal Value v:vs
                                                                                          , t==Value || t==Address])
                            addActives (Branch (SymVal Value sym) _) act = S.insert sym act
                            addActives (Bind bv v) act = maybe id S.insert v $ act S.\\ S.fromList (bindSyms bv)
                            addActives _ act = act
            clobbers i v = ifEmpty (S.singleton v) $ R.lookupRan v (clobbersA!i)
            clobbersA = treeArray next (insertManyA (R.singleton worldID worldID)
                                        [(bindSym bv,s) | bv <- maybe id (:) retVar args
                                                        , bv' <- bindNodes bv
                                                        , s <- bindSyms bv'])
              where next i r (Bind bv v) = insertManyA r _assocs
                      where _assocs = [(bindSym bv',s) | bv' <- bindNodes bv, s <- bindSyms bv]
                                     ++[(s,s') | vv <- maybeToList v
                                               , ref <- S.toList $ references i vv
                                               , s <- S.toList $ clobbers i ref
                                               , s' <- bindSyms bv]
                    next i r (Call _ _ _args) = insertManyA r [
                      a | ref <- S.toList (argsRefs i _args)
                        , v <- S.toList (clobbers i ref)
                        , a <- [(v,worldID),(v,v)]]
                    next _ r _ = r
                    insertManyA r as = insertManyR r [a | (x,y) <- as, a <- [(x,y),(y,x)]]
            lookupRefs v r = ifEmpty (S.singleton worldID) $ R.lookupRan v r
            argsRefs i vs = S.fromList [s | SymVal Address s <- vs]
                           <> S.unions [references i s | SymVal Value s <- vs]
            references i v = lookupRefs v (referencesA!i)
            referencesA = treeArray next $ R.setDom worldID (S.fromList $ concatMap bindSyms args) R.empty
              where next i r (Op _ v vs) = R.setRan v (argsRefs i vs) r
                    next i r (Call d f vs) = R.setRan d (argsRefs i (f:vs) <> S.singleton worldID) r
                    next _ r _ = r

ifEmpty def s | S.null s = def
              | otherwise = s

constA bs v = accumArray const v bs []
zipWithA f a b = array (bounds a) [(i,f x y) | (i,x) <- assocs a | y <- elems b]
applyA = zipWithA ($)
insertManyR = foldl (\r (a,b) -> R.insert a b r)
insertMany = foldl (\m (k,v) -> M.insert k v m)

saturate fun nexts prevs _init start = f _init (array (bounds _init) [(i,length $ prevs i) | i <- indices _init]) start
  where f a _ [] = a
        f a d (i:t) | newElt == a!i = f a d t
                    | otherwise = f (a//[(i,newElt)]) d'' (foldr (insertBy (comparing (d''!))) (filter (/=i) t) (nexts i))
          where newElt = fun i a
                d' = d // [(i,length $ (prevs`asTypeOf`nexts) i)]
                d'' = d' // [(n,(d'!n)-1) | n <- nexts i]
