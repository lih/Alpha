module Specialize(specialize) where

import PCode
import Context.Types
import Data.Word
import Specialize.Architecture
import ID
import Data.ByteString

retCode = exit
  where ret = [0xC3]
        exit = [0xB8, 0x01,0x00,0x00,0x00
               ,0xcd,0x80]        

specialize :: Monad m => Architecture -> (ID -> m Int) -> Code -> (Int,m ByteString)
specialize arch assoc (Code args code ret) = (Prelude.length retCode,return $ pack retCode)
