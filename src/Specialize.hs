module Specialize(specialize) where

import PCode
import Context.Types
import Data.Word
import Specialize.Architecture
import ID
import qualified Data.ByteString as B
import My.Control.Monad
import Data.Array
import My.Control.Monad.State
import My.Data.Tree

retCode = ret
  where ret = [0xc3]
        exit = [0x31,0xdb
               ,0x31,0xc0, 0xff,0xc0
               ,0xcd,0x80]        

specialize :: Monad m => Architecture -> (ID -> m Int) -> Code -> (Int,m B.ByteString)
specialize arch assoc (Code args code ret) = (sum sizes,B.concat $< sequence codes)
  where (sizes,codes) = unzip [(s,c) | (_,s,c) <- elems instructions]
        instructions = execState (mapM_ specializeBranch (branches codeTree)) initialArray
          where specializeBranch b = undefined
        codeTree = spanningTree 0 nexts
        (bounds,instr,nexts,_) = navigate code
        initialArray = array bounds undefined
