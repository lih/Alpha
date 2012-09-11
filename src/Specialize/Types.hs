module Specialize.Types(
  module Data.Word, module Util.TimeLine,
  module IR.Instruction, module Util.ID,
  Register(..),Address(..), 
  Architecture(..), 
  Past(..),Future(..),
  Allocate(..)) where

import Data.Word
import Util.TimeLine
import IR.Instruction
import Util.ID
import Data.Map
import Data.Set

type Register = Int
type Address = (Maybe ID,Int)
data Architecture = Arch {
  archName :: String,
  registers :: Set Register,
  regSize :: Int,
  compileInstr :: Instruction -> Allocate [Word8]
  }
data Past = Past { 
  architecture :: Architecture,
  isActive :: Bool,

  addresses :: Map ID Address,
  bindings :: Map ID Register,
  clobber :: Map ID [ID],
  stack :: [(Bool,Int)]
  }
data Future = Future {
  fregs :: Map ID Register
  }
type Allocate = TimeLine Past Future
