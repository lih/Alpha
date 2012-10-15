{-# LANGUAGE RankNTypes #-}
module Specialize.Types(
  module Data.Word, module My.Control.Monad.TimeLine,
  module PCode, module ID,
  Register(..),Address(..), 
  Architecture(..), 
  Past(..),Future(..),
  Allocate(..)) where

import Data.ByteString
import Data.Word
import My.Control.Monad.TimeLine
import PCode
import ID
import Data.Map
import Data.Set

type Register = Int
type Address = (Maybe ID,Int)
data Architecture = Arch {
  archName           :: String,
  archDefaultSize    :: Int,
  archInitialPast    :: [BindVar] -> Past,
  archCompileCase    :: [Int] -> (Int -> (Int,Int,Past)) -> AllocInstr,
  archCompileBuiltin :: Builtin -> ID -> [Value] -> AllocInstr
  }
data Past = Past { 
  addresses :: Map ID Address,
  bindings  :: Map ID Register,
  clobber   :: Map ID [ID],
  stack     :: [(Bool,Int)]
  }
data Future = Future {
  fregs :: Map ID Register
  }
type Allocate = TimeLine Past Future
type AllocInstr = Monad m => Allocate (Int,Int,m ByteString)
