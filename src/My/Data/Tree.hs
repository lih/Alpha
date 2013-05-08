module My.Data.Tree(module Data.Tree
                   ,nubT,iterateT
                   ,spanningTree
                   ,branches, nodeList
                   ,descend, descendM) where

import Data.Tree
import qualified Data.Set as S
import Control.Monad.State

nubT :: Ord a => Tree a -> Tree a
nubT t = head $ evalState (nubNode t) S.empty
  where nubNode (Node a subs) = gets (S.member a) >>= \b -> if b then return [] else do
          modify (S.insert a)
          subs' <- mapM nubNode subs
          return [Node a (concat subs')]
iterateT :: a -> (a -> [a]) -> Tree a
iterateT seed f = unfoldTree (\a -> (a,f a)) seed

branches :: Tree t -> [[t]]
branches (Node a []) = [[a]]
branches (Node a (sub:subs)) = let (b:t) = branches sub in ((a:b):t++concatMap branches subs)

spanningTree :: Ord a => a -> (a -> [a]) -> Tree a
spanningTree seed nexts = nubT $ iterateT seed nexts

nodeList :: Tree t -> [Tree t]
nodeList n@(Node _ subs) = n:concatMap nodeList subs 

descend :: (t -> t1 -> (a, t1)) -> t1 -> Tree t -> Tree a
descend f s (Node e subs) = Node e' (map (descend f s') subs)
  where (e',s') = f e s
descendM :: Monad m => (b -> a -> m (c, a)) -> a -> Tree b -> m (Tree c)
descendM f s (Node e subs) = do
  (e',s') <- f e s
  subs' <- mapM (descendM f s') subs
  return $ Node e' subs'
