{-# LANGUAGE ParallelListComp #-}
-- |Standard Data.List with some utilities
module My.Data.List(module Data.List,classesBy,nubOrd,sums,zipRest,wrap,count,sortByNat) where

import Data.List

-- |A version of 'nub' optimized for Ord types (/O(n*log(n))/ instead of /O(n^2)/)
nubOrd :: Ord a => [a] -> [a]
nubOrd = map head . group . sort

-- |Returns the successive sums of the given list.
sums :: Num a => [a] -> [a]
sums = scanl (+) 0

-- |@classesBy (==) l@ returns a list of the equivalence classes of (==) by the elements of l.
classesBy _ []    = []
classesBy eq (e:l) = let (x,t) = partition (eq e) l in (e:x):classesBy eq t

-- |@consume f l@ executes @f@ on @l@ until it is empty. 
consume _ [] = []
consume f l = let (a,t) = f l in a:consume f t

-- |@wrap words unwords n l@ wraps @l@ on a word boundary at a maximum size of 
-- @n@ characters per line. Returns a list of the wrapped lines.
wrap :: (a -> [[b]]) -> ([[b]] -> c) -> Int -> a -> [c]
wrap words_ unwords_ n s = consume group_ $ words_ s
  where group_ ws = last [(unwords_ g,t)
                         | g <- inits ws
                         | t <- tails ws
                         | sz <- map (subtract 1) $ sums (map ((1+) . length) ws), sz <= n]

-- |@zipRest l l'@ zips @l@ and @l'@ and also returns the remainder of @l'@ (the elements not zipped)
zipRest l l' = (zip l l',drop (length l) l')

-- |Counts the elements of a list that satisfy the given predicate
count p = length . filter p

-- |Lazy sorting function optimized for use on non-negative consecutive integers
sortByNat from l = sortByEnum 0 l
  where sortByEnum n _l = h ++ sortByEnum (n+1) t
          where (h,t) = partition ((==n) . from) _l 
