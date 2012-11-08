{-# LANGUAGE RankNTypes #-}
module Specialize.Types(
  module Data.Word, module My.Control.Monad.TimeLine,
  module PCode, module ID,module Specialize.Frame,
  Register(..),
  Architecture(..),
  Info(..), Past(..),Future(..),
  frameF, bindingsF, registersF, emptyFuture) where

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
import Specialize.Frame

type Register = Int
data Architecture = Arch {
  archName         :: String,
  archDefaultSize  :: Int,
  archInitials     :: [BindVar] -> BindVar -> (Past,Future),
  archCompileInstr :: Instruction -> ReaderT Info (TimeLine Past Future) (Int,Int,IO ByteString)
  }
data Past = Past {
  registers  :: Bimap ID Register,
  frame :: Frame
  }
          deriving Show
data Future = Future {
  fregisters :: Bimap ID Register
  }
            deriving Show
data Info = Info {
  envInfo    :: (ID,ID -> IO Int),
  bindings   :: Map ID (ID,Int),
  sizes      :: Map ID Int,
  actives    :: Set ID,
  clobbers   :: Relation ID ID,
  locals     :: Set ID,
  branchPos  :: (Int,Int -> (Int,Int,Maybe Past))
  }

frameF = Field (frame,\f p -> p { frame = f })
registersF = Field (registers,\r p -> p { registers = r })
bindingsF = Field (bindings,\a p -> p { bindings = a })

emptyFuture = Future Data.Bimap.empty
