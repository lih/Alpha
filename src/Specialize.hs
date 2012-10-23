{-# LANGUAGE RankNTypes, ParallelListComp, TupleSections #-}
module Specialize(specialize) where

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

specialize arch assoc (Code args code retVar) = seq (unsafePerformIO $ print code >> print (fmap actives infos)) foo
  where 
    foo = (sum sizes,B.concat $< sequence codes)
    -- ret = (length retCode, return (B.pack retCode))
    (estimates,sizes,codes) = unzip3 [v | Right (_,v) <- elems instructions]
    (past,future) = archInitials arch args retVar
    (bounds,instr,nexts,prevs) = navigate code
    codeTree = spanningTree 0 nexts
    runInstr i (p,f) = runTimeLine (runReaderT (compile $ instr i) (infos!i)) (p,f)
      where compile = archCompileInstr arch assoc
    
    instructions = execState (specializeTree past codeTree) initialArray
      where 
        specializeTree p (Node i subs) = gets ((!i) >>> fromLeft) >>= \f -> do
          let newVal@(p',_,vals) = runInstr i (p,f)
          modify (// [(i,Right (p,vals))])
          mapM_ (specializeTree p') subs
        initialArray = array bounds (concatMap f $ branches codeTree)
          where f br = (n,Left fut):[(i,Left f') 
                                    | (i,j) <- zip br (tail br) 
                                    , let (_,f',_) = runInstr j (undefined,f)
                                          Left f = initialArray ! j]
                  where n = last br ; fut = if null (nexts n) then future else emptyFuture
                        
    infos = array bounds (fmap (\(k,i) -> (k,i { actives = act!k })) assocs)
      where assocs = (0,info):concatMap f (nodeList codeTree)
            info = Info M.empty R.empty S.empty
            f (Node i subs) = [(j,merge (infos!i) (instr j)) | Node j _ <- subs] 
            merge (Info bnd refs _) i = Info (newBnd i) (newRefs i) undefined
              where newBnd (Bind bv (Just id)) = foldl (\m (k,v) -> M.insert k v m) bnd 
                                                 [(s,(Just id,n)) | (s,n,_) <- flattenBind (archDefaultSize arch) bv]
                    newBnd _ = bnd
                    newRefs (Op _ v vs) = foldl (\r (a,b) -> R.insert a b r) refs (map (v,) relList)
                      where relList = [s | SymVal Address s <- vs] ++ S.toList (S.unions [fromMaybe S.empty (R.lookupDom s refs) | SymVal Value s <- vs])
                    newRefs _ = refs
            act = saturate fun prevs nexts init start
              where init = accumArray const S.empty bounds [] 
                    start = concat [prevs i | i <- indices infos, isRet (instr i)]
                    fun i a = addActives (instr i) $ S.unions (map (a!) (nexts i))
                      where addActives (Op _ v vs) s = foldr S.insert (S.delete v s) [s | SymVal Value s <- vs]
                            addActives (Branch (SymVal Value id) _) s = S.insert id s
                            addActives (Bind bv v) s = maybe id S.insert v $ foldr S.delete s (bindSyms bv)
                            addActives _ s = s

saturate fun nexts prevs init start = f init (array (bounds init) [(i,length $ prevs i) | i <- indices init]) start
  where f a d [] = a
        f a d (i:t) | newElt == a!i = f a d t
                    | otherwise = f (a//[(i,newElt)]) d'' (foldr (insertBy (comparing (d''!))) (filter (/=i) t) (nexts i))
          where newElt = fun i a 
                d' = d // [(i,length $ (prevs`asTypeOf`nexts) i)]
                d'' = d' // [(n,(d'!n)-1) | n <- nexts i]
