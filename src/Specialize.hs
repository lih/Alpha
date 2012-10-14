module Specialize(specialize) where

import PCode
import Context.Types
import Data.Word
import Specialize.Architecture
import ID
import Data.ByteString

retCode = ret
  where ret = [0xc3]
        exit = [0x31,0xdb
               ,0x31,0xc0, 0xff,0xc0
               ,0xcd,0x80]        

specialize :: Monad m => Architecture -> (ID -> m Int) -> Code -> (Int,m ByteString)
specialize arch assoc (Code args code ret) = (Prelude.length retCode,return $ pack retCode)
