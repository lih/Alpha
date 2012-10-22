{-# LANGUAGE RankNTypes, ParallelListComp, TupleSections #-}
module Specialize(specialize) where

import PCode
import Context.Types
import Data.Word
import Specialize.Architecture
import Specialize.Types
import ID
import qualified Data.ByteString as B
import My.Control.Monad
import Data.Array
import My.Control.Monad.State
import My.Data.Tree
import My.Control.Monad.TimeLine
import Control.Monad.Trans.Reader
import Specialize.Architecture
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Relation as R
import Data.Maybe

retCode = ret
  where ret = [0xc3]
        exit = [0x31,0xdb
               ,0x31,0xc0, 0xff,0xc0
               ,0xcd,0x80]        

specialize arch assoc (Code args code retVar) = foo
  where 
    foo = (sum sizes,B.concat $< sequence codes)
    -- ret = (length retCode, return (B.pack retCode))
    (estimates,sizes,codes) = unzip3 [v | Right (_,v) <- elems instructions]
    (info,past,future) = archInitials arch args retVar
    (bounds,instr,nexts,_) = navigate code
    codeTree = spanningTree 0 nexts
    
    instructions = execState (specializeTree past codeTree) initialArray
      where 
        specializeTree p (Node i subs) = do
          a <- get
          let newVal@(p',_,vals) = runInstr i (p,f) ; Left f = a!i
          put (a // [(i,Right (p,vals))])
          mapM_ (specializeTree p') subs
        runInstr i (p,f) = runTimeLine (runReaderT (archCompileInstr arch $ instr i) (infos!i)) (p,f)
        initialArray = array bounds (concatMap f $ branches codeTree)
          where f br = (n,Left fut):[(i,Left f') 
                                    | (i,j) <- zip br (tail br) 
                                    , let (_,f',_) = runInstr j (undefined,f)
                                          Left f = initialArray ! j]
                  where n = last br ; fut = if null (nexts n) then future else emptyFuture
                    
    infos = array bounds assocs
      where assocs = (0,info):concatMap f (nodeList codeTree)
            f (Node i subs) = [(j,merge (infos!i) (instr j)) | Node j _ <- subs] 
            merge (Info bnd cl refs) i = Info (newBnd i) undefined (newRefs i)
              where newBnd (Bind bv (Just id)) = foldl (\m (k,v) -> M.insert k v m) bnd 
                                                 [(s,(Just id,n)) | (s,n,_) <- flattenBind (archDefaultSize arch) bv]
                    newBnd _ = bnd
                    newRefs (Op _ v vs) = foldl (\r (a,b) -> R.insert a b r) refs (map (v,) relList)
                      where relList = [s | SymVal Address s <- vs] ++ S.toList (S.unions [fromMaybe S.empty (R.lookupDom s refs) | SymVal Value s <- vs])
                    newRefs _ = refs

            


