module ID where

newtype ID = ID Int
           deriving (Ord,Eq)
newtype Range a = Range (a,a)

singleRange i = Range (i,i)

instance Ord a => Eq (Range a) where
  a == b = compare a b == EQ
instance Ord a => Ord (Range a) where
  compare (Range (a,b)) (Range (a',b')) =  
    if b<=a' then LT
    else if b'<=a then GT
         else EQ

instance Enum ID where
  toEnum = ID
  fromEnum (ID i) = i
instance Show ID where
  show (ID i) = "x"++show i
instance Num ID where
  ID a + ID b = ID (a+b)
  ID a - ID b = ID (a-b)
  ID a * ID b = ID (a*b)
  negate = undefined
  abs = undefined
  signum = undefined
  fromInteger n = ID $ fromInteger n


