{-# LANGUAGE RankNTypes #-}
module Specialize.Types(module Data.Word, module My.Control.Monad.RWTL
                       ,module PCode, module ID,module Specialize.Frame
                       ,Register(..),BinCode(..), isEmptyCode, binCodeData
                       ,Architecture(..)
                       ,Info(..)
                       ,MemState(..),Future(..), emptyFuture
                       ,frameF, registersF, changedF, fregistersF) where

import Data.Bimap
import Data.Set
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
  show (BC (e,s,_)) = show (e,s)

isEmptyCode (BC (e,_,_)) = e==0
binCodeData (BC (_,_,b)) = b

type Register = Int
data Architecture = Arch {
  archName         :: String,
  archDefaultSize  :: Int,
  archInitials     :: [BindVar] -> Maybe BindVar -> (MemState,Future),
  archCompileInstr :: Instruction -> RWTL Info BinCode MemState Future ()
  }
data MemState = MemState {
  registers :: Bimap ID Register,
  changed   :: Set ID,
  frame     :: Frame
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
  branchPos  :: (Int,Int -> (Int,Int,Maybe MemState))
  }

instance Show Info where
  show (Info _ b sz a c _) = "Info { bindings = "++show b
                               ++", sizes = "++show sz
                               ++", actives = "++show a
                               ++", clobbers = "++show c
                               ++" }"

registersF  = Field (registers  ,\r p -> p { registers = r })
changedF    = Field (changed    ,\c p -> p { changed = c })
frameF      = Field (frame      ,\f p -> p { frame = f })
fregistersF = Field (fregisters ,\r f -> f { fregisters = r })

emptyFuture = Future Data.Bimap.empty
