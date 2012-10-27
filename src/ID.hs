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

-- Copyright (c) 2012, Coiffier Marc <marc.coiffier@gmail.com>
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

--     Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--     Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

