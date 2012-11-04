module My.Data.List(module Data.List,classesBy,nubOrd,sums,zipRest) where

import Data.List

nubOrd :: Ord a => [a] -> [a]
nubOrd = map head . group . sort

classesBy :: (a -> a -> Bool) -> [a] -> [[a]]
classesBy (==) []    = []
classesBy (==) (e:l) = let (x,t) = partition (e==) l in (e:x):classesBy (==) t

sums :: Num a => [a] -> [a]
sums = scanl (+) 0

zipRest l l' = (zip l l',drop (length l) l')

