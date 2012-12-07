{-# LANGUAGE ParallelListComp #-}
module My.Data.List(module Data.List,classesBy,nubOrd,sums,zipRest,wrap) where

import Data.List

nubOrd :: Ord a => [a] -> [a]
nubOrd = map head . group . sort

classesBy :: (a -> a -> Bool) -> [a] -> [[a]]
classesBy (==) []    = []
classesBy (==) (e:l) = let (x,t) = partition (e==) l in (e:x):classesBy (==) t

sums :: Num a => [a] -> [a]
sums = scanl (+) 0

consume f [] = []
consume f l = let (a,t) = f l in a:consume f t

wrap words unwords n s = consume group $ words s
  where group ws = last [(unwords g,t)
                        | g <- inits ws
                        | t <- tails ws
                        | sz <- map (subtract 1) $ sums (map ((1+) . length) ws), sz <= n]

zipRest l l' = (zip l l',drop (length l) l')

