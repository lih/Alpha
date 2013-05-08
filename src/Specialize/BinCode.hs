{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}
-- |A module describing the binary code type
module Specialize.BinCode(
  -- * The BinCode Type
  BinCode(..),binCodeData,isEmptyCode,
  -- ** The Position type
  Position(..),mkPos,posEst,posAddr,
  -- * Binary utilities
  fi,fis,bc,bcM,bcEst,fromFields,bytes,numSize,withSize
  ) where

import qualified Data.ByteString as B
import Data.Word
import Data.Monoid
import Data.Bits
import My.Control.Monad.WriterAcc

-- | A type describing a generated binary code with an estimated size, an actual size and binary data
newtype BinCode = BC (Int,Int,IO B.ByteString)
instance Monoid BinCode where
  mempty = BC (0,0,return mempty)
  mappend (BC (e,s,v)) (BC (e',s',v')) = BC (e+e',s+s',liftM2 (<>) v v')
instance Show BinCode where
  show (BC (e,s,_)) = show (e,s)

-- | A type describing a position in generated code (contains an estimated position and an actual one)
newtype Position = Position (Sum Int,Sum Int)
                 deriving (Monoid,Show)
instance Endomorphic BinCode Position where
  endo (BC (e,a,_)) = Position (Sum e,Sum a)

-- | Is this code empty ?
isEmptyCode (BC (e,_,_)) = e==0
-- | The BinCode's data
binCodeData (BC (_,_,b)) = b

-- | Constructs a Position from an estimate and an actual position
mkPos e s = Position (Sum e,Sum s)
-- | The position estimate
posEst (Position (Sum e,_)) = e
-- | The actual position
posAddr (Position (_,Sum a)) = a

-- | Constructs a BinCode from its three components
bcEst e s c = BC (e,s,liftM B.pack c)
-- | Constructs a BinCode with a known size
bcM n c = bcEst n n c
-- | Constructs a BinCode from a pure list
bc c = bcM (length c) (return c)

-- | Shortened 'fromIntegral'
fi = fromIntegral
-- | Shortened @('fmap' . 'fromIntegral')@
fis = fmap fromIntegral

-- | Returns the minimum number of bits needed to represent a number
numSize n | n>=0 = _numSize 64 n
          | otherwise = 1+_numSize 64 (-1-n)
  where _numSize 0 _ = 1
        _numSize bl _n = case reverse $ takeWhile (>0) $ iterate (`shiftR`bl) _n of
          [] -> 0
          (x:t) -> (length t)*bl + _numSize (bl`div`2) x
-- | Splits a number into bytes
bytes n = fis (iterate (`shiftR`8) n) :: [Word8]
-- | Constructs a number from a list of fields of the form @(value,size)@ where @size@ is in bits.
fromFields fs = fi $ foldl1 xor (zipWith shiftL (map fst fs) (scanl (+) 0 $ map snd fs))
-- | Couples a number with its size
withSize n = (numSize n,return $ fi n)
