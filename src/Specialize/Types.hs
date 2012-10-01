module Specialize.Types(
  module Data.Word, module My.Control.Monad.TimeLine,
  module PCode.Instruction, module ID,
  Register(..),Address(..), 
  Architecture(..), 
  Past(..),Future(..),
  Allocate(..)) where

import Data.Word
import My.Control.Monad.TimeLine
import PCode.Instruction
import ID
import Data.Map
import Data.Set

type Register = Int
type Address = (Maybe ID,Int)
data Architecture = Arch {
  archName         :: String,
  archInitialPast  :: [BindVar] -> Past,
  archFinalFuture  :: Future,
  archCompileInstr :: Instruction -> (Int -> ((Int,Int),Past)) -> Allocate (Int,[Word8])
  }
data Past = Past { 
  addresses :: Map ID Address,
  bindings  :: Maybe (Map ID Register),
  clobber   :: Map ID [ID],
  stack     :: [(Bool,Int)]
  }
data Future = Future {
  fregs :: Map ID Register
  }
type Allocate = TimeLine Past Future
