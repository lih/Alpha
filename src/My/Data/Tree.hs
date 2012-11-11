module My.Data.Tree(module Data.Tree
                   ,nubT,iterateT
                   ,spanningTree
                   ,branches, nodeList, descend) where

import Data.Tree
import qualified Data.Set as S
import Control.Monad.State

nubT t = evalState (unfoldTreeM unfold t) S.empty
  where unfold (Node a subs) = do
          modify (S.insert a) ; s <- get
          return (a,[sub | sub <- subs, not (S.member (rootLabel sub) s)]) 
iterateT seed f = unfoldTree (\a -> (a,f a)) seed

branches (Node a []) = [[a]]
branches (Node a (sub:subs)) = let (b:t) = branches sub in ((a:b):t++concatMap branches subs)

spanningTree seed nexts = nubT $ iterateT seed nexts

nodeList n@(Node _ subs) = n:concatMap nodeList subs 

descend f s (Node e subs) = Node e' (map (descend f s') subs)
  where (e',s') = f e s
