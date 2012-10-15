module My.Data.Tree(module Data.Tree
                   ,nubT,iterateT
                   ,spanningTree
                   ,branches) where

import Data.Tree
import Data.Set as S
import Control.Monad.State

nubT t = evalState (unfoldTreeM unfold t) S.empty
  where unfold (Node a subs) = do
          modify (S.insert a) ; s <- get
          return (a,[sub | sub <- subs, not (S.member (rootLabel sub) s)]) 
iterateT seed f = unfoldTree (\a -> (a,f a)) seed

branches (Node a []) = [[a]]
branches (Node a (sub:subs)) = let (b:t) = branches sub in ((a:b):t++concatMap branches subs)

spanningTree seed nexts = nubT $ iterateT seed nexts

test = Node 1 [Node 2 [Node 3 [],Node 4 []],
               Node 5 [Node 6 [],Node 7 []]]
