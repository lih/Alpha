module Specialize(specialize) where

import PCode
import Context.Types
import Data.Word
import Specialize.Architecture
import ID
import Data.ByteString

specialize :: Monad m => Architecture -> (ID -> m Int) -> Code -> (Int,m ByteString)
specialize arch assoc (Code args code ret) = (1,return $ pack [0x83])
