{-# LANGUAGE RankNTypes #-}
module Specialize.Types(
  module Data.Word, module My.Control.Monad.TimeLine,
  module PCode, module ID,
  Register(..),Address(..), 
  Architecture(..), 
  Info(..), Past(..),Future(..),
  stackF,bindingsF, emptyFuture) where

import Data.Relation
import Data.ByteString
import Data.Word
import My.Control.Monad.TimeLine
import My.Control.Monad.State
import Control.Monad.Trans.Reader
import PCode
import ID
import Data.Map
import Data.Set

type Register = Int
type Address = (Maybe ID,Int)
data Architecture = Arch {
  archName         :: String,
  archDefaultSize  :: Int,
  archInitials     :: [BindVar] -> BindVar -> (Past,Future),
  archCompileInstr :: (ID -> IO Int) -> Instruction -> ReaderT Info (TimeLine Past Future) (Int,Int,IO ByteString)
  }
data Past = Past { 
  registers  :: Map ID Register,
  stackAddrs :: Map ID Address,
  stack      :: [(Bool,Int)]
  }
data Future = Future {
  fregisters :: Map ID Register
  }
data Info = Info {
  bindings   :: Map ID Address,
  references :: Relation ID ID,
  actives    :: Set ID
  }
          deriving Show

stackF = Field (stack,\s p -> p { stack = s })
bindingsF = Field (bindings,\a p -> p { bindings = a })

emptyFuture = Future Data.Map.empty