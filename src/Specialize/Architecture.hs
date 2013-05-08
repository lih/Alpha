{-# LANGUAGE ImplicitParams, Rank2Types #-}
module Specialize.Architecture where

import PCode
import Specialize.Monad
import Specialize.Memory
import Specialize.Info

data Architecture = Arch {
  archName         :: String,
  archDefaultSize  :: Int,
  archInitials     :: [BindVar] -> Maybe BindVar -> (MemState,FutureState),
  archCompileInstr :: ( ?info :: Info ) => Instruction -> Specialize ()
  }
instance Eq Architecture where
  a == a' = archName a == archName a'
instance Show Architecture where
  show a = "#<Arch:"++archName a++">"

