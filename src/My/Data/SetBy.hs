module My.Data.SetBy (SetBy
                     ,empty,fromList,fromAscList
                     ,null
                     ,toList,toAscList
                     ,insert,delete,deleteMany,member
                     ,findMin
                     ,partition
                     ,union) where

import Prelude hiding (null)
import qualified Data.Tree.AVL as AVL
import Data.COrdering
import Data.Maybe
import qualified Data.List as L

data SetBy a = SetBy {
  cmpFunc :: a -> a -> COrdering a,
  tree :: AVL.AVL a
  }

instance Show a => Show (SetBy a) where
  show (SetBy _ t) = case AVL.asListL t of
    [] -> "empty"
    l -> "fromList "++show l

empty cmp = SetBy (fstByCC cmp) AVL.empty
fromList cmp l = fromAscList cmp (L.sortBy cmp l)
fromAscList = fromAscList' . fstByCC
fromAscList' cmp l = SetBy cmp (AVL.asTreeL l)

null = AVL.isEmpty . tree

toList = toAscList
toAscList (SetBy _ t) = AVL.asListL t

insert e (SetBy cmp t) = SetBy cmp (AVL.push (cmp e) e t)
delete e (SetBy cmp t) = SetBy cmp (fromMaybe t $ fmap snd $ AVL.tryPop (cmp e) t)
deleteMany l s = foldr delete s l
member e (SetBy cmp t) = isJust $ AVL.tryRead t (cmp e)

findMin (SetBy _ t) = AVL.assertReadL t

partition p s@(SetBy c t) = (fromAscList' c a,fromAscList' c b)
  where ~(a,b) = L.partition p $ toAscList s

union (SetBy c t) (SetBy _ t') = SetBy c (AVL.union c t t')
