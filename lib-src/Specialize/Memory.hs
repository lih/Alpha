{-# LANGUAGE ViewPatterns #-}
module Specialize.Memory(
  -- * Memory states
  MemState(..),FutureState(..),emptyFutureState,
  -- * Locations and constants
  RegID,
  Location(..),isFlags,isRegister,
  Constant(..),applyC,toEnv,
  -- * Views
  locations_,frame_,flocations_,
  -- * Utilities
  varRegs
  ) where

import ID
import Data.Ord
import Specialize.Frame
import My.Data.Relation as R
import Data.Set as S
import Data.View
import Control.Monad (liftM,liftM2)

-- | A type describing a compile-time constant
data Constant = EnvConst (IO Int) -- ^ An environment constant (i.e. the address of a global symbol)
              | AbsConst Integer  -- ^ An absolute constant
instance Show Constant where
  show (EnvConst _) = "<env>"
  show (AbsConst n) = show n
-- | Lifts a binary operator to the Constant type
applyC op = apply
  where apply (AbsConst n) (AbsConst n') = AbsConst (add n n')
        apply c c' = EnvConst (liftM2 add (fromC c) (fromC c'))
          where fromC (EnvConst n) = liftM fromIntegral n
                fromC (AbsConst n) = return (fromIntegral n)
        add a b = fromIntegral $ (fromIntegral a `op` fromIntegral b)
-- | Turns a constant into an environment value
toEnv (EnvConst c) = c
toEnv (AbsConst n) = return (fromIntegral n)

{-|
A type describing all possible locations of a variable (in memory,in a register,
a constant or in the machine flags)
-}
data Location = Register RegID
              | Memory
              | Constant Constant
              | Flags Int
              deriving Show
type RegID = Int
instance Eq Location where
  a == b = compare a b == EQ
instance Ord Location where
  compare = comparing value
    where value (Register r) = (0,Just $ fromIntegral r)
          value Memory = (1,Nothing)
          value (Constant (AbsConst n)) = (2,Just n)
          value (Constant (EnvConst _)) = error "Cannot compare against a constant environment value"
          value (Flags _) = (3,Nothing)
isFlags (Flags _) = True ; isFlags _ = False
isRegister (Register _) = True; isRegister _ = False
flagsLoc = Flags undefined

-- |A type describing the state of the whole memory at a given time.
data MemState = MemState {
  -- | A relation between variables and their locations. @var '-?-' loc@ iff @var@ is present at location @loc@
  locations     :: Relation ID Location,
  -- | The current stack frame
  frame         :: Frame
  }
          deriving Show
-- | The future memory state
data FutureState = FutureState {
  -- | The future locations of some variables
  flocations :: Relation ID Location
  }
            deriving Show
-- |The empty future state
emptyFutureState = FutureState R.empty

flocations_ = View flocations (\r f -> f { flocations = r })
locations_  = View locations  (\r p -> p { locations = r })
frame_      = View frame      (\f p -> p { frame = f })

-- |A list of all the registers associated with a variable in a memory state relation.
varRegs s l = [r | Register r <- S.toList (R.lookupRan s l)]
