{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ID where

newtype ID = ID Int
           deriving (Ord,Eq,Enum,Num)
newtype Range a = Range (a,a)

singleRange i = Range (i,i)

instance Ord a => Eq (Range a) where
  a == b = compare a b == EQ
instance Ord a => Ord (Range a) where
  compare (Range (a,b)) (Range (a',b'))
    | b<=a' = LT
    | b'<=a = GT 
    | otherwise = EQ

instance Show ID where
  show (ID i) = "x"++show i

