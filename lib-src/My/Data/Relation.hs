{-# LANGUAGE NoMonomorphismRestriction #-}
-- |A personal implementation of a finite relation between type
-- (I didn't quite like the interface of the Data.Relation module)
module My.Data.Relation(
  -- * The Relation Type
  Relation,
  -- * Contructing Relations
  empty,singleton,fromList,toList,
  inverse,
  -- * Inserting/Deleting associations
  insert,delete,
  -- * Testing for membership
  member,notMember,
  -- * Operations on whole ranges/domains
  lookupRan,lookupDom,range,domain,
  modifyRan,modifyDom,setRan,setDom,
  filterDom,filterRan,
  -- * Operators
  (-?<),(>?-),
  (<!-),(-!>),(-?-),(-!-),(-+-),(-/-)
  ) where

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

-- |The Relation type
data Relation a b = Relation {
  ranges :: M.Map a (S.Set b),
  domains :: M.Map b (S.Set a)
  }
                  deriving Show

-- |The empty Relation
empty = Relation M.empty M.empty
-- |The singleton Relation 
singleton a b = insert a b empty
-- |Contructs a Relation from a list of associations
fromList = foldr (uncurry insert) empty
-- |Destructs a Relation into a list ('fromList . toList = id')
toList r = [(a,b) | (a,bs) <- M.assocs (ranges r), b <- S.toList bs]

-- |/O(1)/. The inverse relation.
inverse (Relation a b) = Relation b a

insertVal a b m = M.alter (Just . S.insert b . fromMaybe S.empty) a m
deleteVal a b m = M.alter (mfilter (not . S.null) . fmap (S.delete b)) a m

-- |Inserts an association into the Relation
insert a b (Relation ran dom) = Relation (insertVal a b ran) (insertVal b a dom)
-- |Deletes an association from the Relation. If the association was not present, the original Relation is returned
delete a b (Relation ran dom) = Relation (deleteVal a b ran) (deleteVal b a dom)

-- |Tests for association within the given Relation
member a b r = S.member b (lookupRan a r)
-- |The opposite of 'member'
notMember a b = not . member a b

-- |The range of a value
lookupRan a r = fromMaybe S.empty $ M.lookup a (ranges r)
-- |The range of all values
range = S.unions . M.elems . ranges
-- |The domain of a value
lookupDom b = lookupRan b . inverse
-- |The domain of all values
domain = range . inverse

-- |Applies the given function to the range of the given value
modifyRan f a r = r {
  ranges = M.alter (const $ if S.null newRan then Nothing else Just newRan) a (ranges r),
  domains = execState st (domains r)
  }
  where newRan = f oldRan ; oldRan = lookupRan a r
        st = sequence_ [modify (deleteVal b a) | b <- S.toList $ oldRan S.\\ newRan]
             >> sequence_ [modify (insertVal b a) | b <- S.toList $ newRan S.\\ oldRan]
-- |Applies the given function to the domain of the given value
modifyDom f b r = inverse (modifyRan f b (inverse r))

-- |Sets a value's range
setRan a = flip modifyRan a . const
-- |Sets a value's domain
setDom b = flip modifyDom b . const

-- |Filters a value's range by the given predicate
filterRan p r = inverse (filterDom p (inverse r))
-- |Filters a value's domain by the given predicate
filterDom p (Relation ran dom) = Relation (M.filterWithKey (const . p) ran) (M.map (S.filter p) dom)

-- |Infix flipped alias to lookupRan
(-?<) = flip lookupRan
-- |Infix flipped alias to lookupDom
(>?-) = flip lookupDom

-- |Infix alias to 'member'
(-?-) = member
-- |Infix alias to 'setRan'
(-!>) = setRan
-- |Infix flipped alias to 'setDom'
(<!-) = flip setDom
-- |Infix alias to 'insert'
(-+-) = insert
-- |Infix alias to 'delete'
(-/-) = delete
-- |Associates two values exclusively to one another
a -!- b = (S.singleton a <!- b) . (a -!> S.empty)
