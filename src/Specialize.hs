{-# LANGUAGE RankNTypes, ParallelListComp, TupleSections #-}
module Specialize(specialize) where

import Data.Monoid
import Control.Arrow
import Context.Types
import Control.Monad.Trans.Reader
import Data.Array
import Data.List
import Data.Maybe
import Data.Word
import Data.Ord
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Relation as R
import qualified Data.Set as S
import ID
import My.Control.Monad
import My.Control.Monad.State
import My.Control.Monad.TimeLine
import My.Data.Either
import My.Data.Tree
import PCode
import Specialize.Architecture
import Specialize.Architecture
import Specialize.Types

import System.IO.Unsafe
import My.Prelude

retCode = ret
  where ret = [0xc3]
        exit = [0x31,0xdb
               ,0x31,0xc0, 0xff,0xc0
               ,0xcd,0x80]        

sums = scanl (+) 0

specialize arch assoc (Code args code retVar) = foo
  where 
    foo = (sum sizes,B.concat $< sequence codes)
    -- foo = (length retCode, return (B.pack retCode))
    (estimates,sizes,codes) = unzip3 [v | Right (_,v) <- elems instructions]
    (past,future) = archInitials arch args retVar
    (bounds,instr,nexts,prevs) = navigate code
    positions = listArray bounds [(e,s) | e <- sums estimates, s <- sums sizes]
    codeTree = spanningTree 0 nexts
    runInstr i (p,f) past = runTimeLine (runReaderT (compile $ instr i) ((infos!i) getPos)) (p,f)
      where compile = archCompileInstr arch
            getPos j = (e'-e,d'-d,past j)
              where (e,d) = positions!i ; (e',d') = positions!j
                    
    treeArray next seed = ret
      where assocs = (0,seed):concatMap f (nodeList codeTree)
            ret = array bounds assocs
            f (Node i subs) = [(j,next j (ret!i) (instr j)) | Node j _ <- subs] 
        
    instructions = execState (specializeTree past codeTree) initialArray
      where 
        specializeTree p (Node i subs) = gets ((!i) >>> fromLeft) >>= \f -> do
          past <- gets (!)
          let newVal@(p',_,vals) = runInstr i (p,f) (either (const Nothing) (Just . fst) . past)
          modify (// [(i,Right (p,vals))])
          mapM_ (specializeTree p') subs
        initialArray = fmap Left init 
          where init = array bounds (concatMap f $ branches codeTree)
                f br = (n,fut):[(i,snd3 $ runInstr j (undefined,init!j) (const Nothing)) | (i,j) <- zip br (tail br)]
                  where n = last br ; fut = if null (nexts n) then future else emptyFuture
                        
    infos = constA bounds (Info assoc) `applyA` bindingsA `applyA` activesA `applyA` clobbersA
      where parent i v = fmap fst $ M.lookup v (bindingsA!i)
            bindingsA = treeArray next M.empty
              where next _ bnd (Bind bv (Just id)) = foldl (\m (k,v) -> M.insert k v m) bnd 
                                                    [(s,(id,n)) | (s,n,_) <- flattenBind (archDefaultSize arch) bv]
                    next _ bnd _ = bnd
            activesA = saturate fun prevs nexts init start
              where init = accumArray const S.empty bounds [] 
                    start = concat [prevs i | i <- indices init, isRet (instr i)]
                    fun i a = addActives (instr i) $ S.unions (map (a!) (nexts i))
                      where addActives (Op _ v vs) s = (s S.\\ clobbers i v) 
                                                       <> S.unions [clobbers i s' | SymVal Value s <- vs 
                                                                                  , s' <- s:maybeToList (parent i s)]
                                                       <> maybe S.empty (clobbers i) (parent i v)
                            addActives (Branch (SymVal Value id) _) s = s <> clobbers i id
                            addActives (Bind bv v) s = maybe id S.insert v $ s S.\\ S.fromList (bindSyms bv)
                            addActives _ s = s
            clobbers i v = fromMaybe (S.singleton v) $ R.lookupRan v (clobbersA!i)                
            clobbersA = treeArray next (foldl (next undefined) R.empty [Bind bv Nothing | bv <- retVar:args])
              where next i r (Bind bv v) = insertManyR r' assocs
                      where r' = restrict r (S.fromList (bindSyms bv))
                            assocs = [ass | bv <- bindNodes bv
                                          , s <- bindSyms bv
                                          , ass <- [(bindSym bv,s),(s,bindSym bv)]]
                                     ++[ass | v <- maybeToList v
                                            , ref <- S.toList $ references i v
                                            , s <- maybe [v] S.toList (R.lookupRan ref r')
                                            , ass <- [(s,ref),(ref,s)]]
                    next _ r _ = r
            lookupRefs v r = fromMaybe (S.singleton (ID (-1))) $ R.lookupRan v r
            references i v = lookupRefs v (referencesA!i)
            referencesA = treeArray next R.empty
              where next i r (Op _ v vs) = insertManyR r' (map (v,) $ S.toList refs)
                      where r' = S.delete v (R.dom r) R.<| r
                            refs = S.fromList [s | SymVal Address s <- vs] 
                                   <> S.unions [lookupRefs v r | SymVal Value s <- vs]
                    next _ r (Bind bv _) = restrict r (S.fromList (bindSyms bv))
                    next _ r _ = r

restrict r s = (R.dom r S.\\ s) R.<| r R.|> (R.ran r S.\\ s)

constA bs v = accumArray const v bs []
zipWithA f a b = array (bounds a) [(i,f x y) | (i,x) <- assocs a | y <- elems b]
applyA = zipWithA ($)
insertManyR = foldl (\r (a,b) -> R.insert a b r)

saturate fun nexts prevs init start = f init (array (bounds init) [(i,length $ prevs i) | i <- indices init]) start
  where f a d [] = a
        f a d (i:t) | newElt == a!i = f a d t
                    | otherwise = f (a//[(i,newElt)]) d'' (foldr (insertBy (comparing (d''!))) (filter (/=i) t) (nexts i))
          where newElt = fun i a 
                d' = d // [(i,length $ (prevs`asTypeOf`nexts) i)]
                d'' = d' // [(n,(d'!n)-1) | n <- nexts i]
