{-# LANGUAGE RankNTypes #-}
module Specialize.Types(
  module Data.Word, module My.Control.Monad.TimeLine,
  module PCode, module ID,
  Register(..),
  Architecture(..),
  Info(..), Past(..),Future(..),
  stackF,bindingsF, emptyFuture) where

import Control.Monad.Trans.Reader
import Data.Bimap
import Data.ByteString
import Data.Map
import Data.Relation
import Data.Set
import Data.Word
import ID
import My.Control.Monad.State
import My.Control.Monad.TimeLine
import PCode

type Register = Int
data Architecture = Arch {
  archName         :: String,
  archDefaultSize  :: Int,
  archInitials     :: [BindVar] -> BindVar -> (Past,Future),
  archCompileInstr :: Instruction -> ReaderT Info (TimeLine Past Future) (Int,Int,IO ByteString)
  }
data Past = Past {
  registers  :: Bimap ID Register,
  stackAddrs :: Map ID Int,
  stack      :: [(Bool,Int)]
  }
          deriving Show
data Future = Future {
  fregisters :: Map ID Register
  }
            deriving Show
data Info = Info {
  envInfo    :: (ID,ID -> IO Int),
  bindings   :: Map ID (ID,Int),
  sizes      :: Map ID Int,
  actives    :: Set ID,
  clobbers   :: Relation ID ID,
  branchPos  :: Int -> (Int,Int,Maybe Past)
  }

stackF = Field (stack,\s p -> p { stack = s })
bindingsF = Field (bindings,\a p -> p { bindings = a })

emptyFuture = Future Data.Map.empty