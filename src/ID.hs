{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ID where

newtype ID = ID { unID :: Int }
           deriving (Ord,Eq,Enum,Num)
-- | A range of shape (min,max) of ordered values.
newtype Range a = Range (a,a)

-- | The unit range
unitRange i = Range (i,i)

-- | @r < r'@ iff all values of @r@ are less than any value of @r'@
instance Ord a => Ord (Range a) where
  compare (Range (a,b)) (Range (a',b'))
    | b<a' = LT
    | b'<a = GT 
    | otherwise = EQ
-- | Range equivalence. Two ranges are equivalent iff they share a common subrange (equivalence in this case is not transitive, so beware)
instance Ord a => Eq (Range a) where
  a == b = compare a b == EQ

instance Show ID where
  show (ID i) = "#"++show i

