{-# LANGUAGE RankNTypes #-}
module Specialize.Types(
  module Data.Word, module My.Control.Monad.TimeLine,
  module PCode, module ID,
  Register(..),
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
data Architecture = Arch {
  archName         :: String,
  archDefaultSize  :: Int,
  archInitials     :: [BindVar] -> BindVar -> (Past,Future),
  archCompileInstr :: (ID -> IO Int) -> Instruction -> ReaderT Info (TimeLine Past Future) (Int,Int,IO ByteString)
  }
data Past = Past { 
  registers  :: Map ID Register,
  stackAddrs :: Map ID Int,
  stack      :: [(Bool,Int)]
  }
data Future = Future {
  fregisters :: Map ID Register
  }
data Info = Info {
  bindings   :: Map ID (ID,Int),
  actives    :: Set ID,
  clobbers   :: Relation ID ID,
  branchInfo :: Int -> (Int,Int,Maybe Past)
  }

stackF = Field (stack,\s p -> p { stack = s })
bindingsF = Field (bindings,\a p -> p { bindings = a })

emptyFuture = Future Data.Map.empty