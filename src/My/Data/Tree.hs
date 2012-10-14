module My.Data.Tree(module Data.Tree
                   ,nubT,iterateT
                   ,spanningTree) where

import Data.Tree
import Data.Set as S
import Control.Monad.State

nubT t = evalState (unfoldTreeM unfold t) S.empty
  where unfold (Node a subs) = do
          modify (S.insert a) ; s <- get
          return (a,[sub | sub <- subs, not (S.member (rootLabel sub) s)]) 
iterateT seed f = unfoldTree (\a -> (a,f a)) seed

spanningTree seed nexts = nubT $ iterateT seed nexts
