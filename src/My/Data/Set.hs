module My.Data.Set (module Data.Set,Ordered(..)) where

import Data.Set

data Ordered a = Ordered a (a -> a -> Ordering)

instance Eq (Ordered a) where
  a == b = compare a b==EQ
instance Ord (Ordered a) where
  compare (Ordered a cmp) (Ordered b _) = cmp a b
