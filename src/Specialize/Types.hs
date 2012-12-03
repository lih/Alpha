{-# LANGUAGE RankNTypes #-}
module Specialize.Types(BinCode(..), isEmptyCode, binCodeData
                       ,Architecture(..)
                       ,Info(..)
                       ,Location(..), isFlags, regSyms, symLocs, symReg
                       ,MemState(..), Future(..), emptyFuture
                       ,frame_, locations_, flocations_) where

import Data.Ord
import Data.Bimap
import Data.Set
import Data.ByteString
import Data.Map
import My.Data.Relation as R
import Data.Set as S
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

data Location = Register Int
              | Memory
              | Constant Integer
              | Flags Int
              deriving Show
instance Eq Location where
  a == b = compare a b == EQ
instance Ord Location where
  compare = comparing value
    where value (Register r) = (0,Just $ fromIntegral r)
          value Memory = (1,Nothing)
          value (Constant n) = (2,Just n)
          value (Flags _) = (3,Nothing)
  
data Architecture = Arch {
  archName         :: String,
  archDefaultSize  :: Int,
  archInitials     :: [BindVar] -> Maybe BindVar -> (MemState,Future),
  archCompileInstr :: Instruction -> RWTL Info BinCode MemState Future ()
  }
data MemState = MemState {
  locations :: Relation ID Location,
  frame     :: Frame
  }
          deriving Show
data Future = Future {
  flocations :: Relation ID Location
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

locations_  = View (locations  ,\r p -> p { locations = r })
frame_      = View (frame      ,\f p -> p { frame = f })
flocations_ = View (flocations ,\r f -> f { flocations = r })

regNum (Register r) = Just r
regNum _ = Nothing
isFlags (Flags _) = True ; isFlags _ = False

regSyms r locs = S.toList $ R.lookupDom (Register r) locs
symLocs s locs = S.toList $ R.lookupRan s locs
symReg s locs = msum [regNum l | l <- symLocs s locs]

emptyFuture = Future R.empty
