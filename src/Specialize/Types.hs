{-# LANGUAGE RankNTypes #-}
module Specialize.Types(module Data.Word, module My.Control.Monad.RWTL
                       ,module PCode, module ID,module Specialize.Frame
                       ,Register(..),BinCode(..), isEmptyCode
                       ,Architecture(..)
                       ,Info(..)
                       ,MemState(..),Future(..), emptyFuture
                       ,frameF, registersF) where

import Data.Bimap
import Data.ByteString
import Data.Map
import Data.Relation
import Data.Set
import Data.Word
import ID
import My.Control.Monad.State
import My.Control.Monad.RWTL
import PCode
import Specialize.Frame

newtype BinCode = BC (Int,Int,IO ByteString)
instance Monoid BinCode where
  mempty = BC (0,0,return mempty)
  mappend (BC ~(e,s,v)) (BC ~(e',s',v')) = BC (e+e',s+s',liftM2 (<>) v v')
instance Show BinCode where
  show (BC (e,a,_)) = show (e,a)

isEmptyCode (BC (e,_,_)) = e==0

type Register = Int
data Architecture = Arch {
  archName         :: String,
  archDefaultSize  :: Int,
  archInitials     :: [BindVar] -> BindVar -> (MemState,Future),
  archCompileInstr :: Instruction -> RWTL Info BinCode MemState Future ()
  }
data MemState = MemState {
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
  branchPos  :: (Int,Int -> (Int,Int,Maybe MemState))
  }

frameF = Field (frame,\f p -> p { frame = f })
registersF = Field (registers,\r p -> p { registers = r })

emptyFuture = Future Data.Bimap.empty
