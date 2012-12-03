{-# LANGUAGE NoMonomorphismRestriction #-}
module My.Data.Relation(Relation
                       ,empty,singleton,fromList,toList
                       ,inverse
                       ,insert,delete
                       ,member,notMember
                       ,lookupRan,lookupDom,range,domain
                       ,modifyRan,modifyDom,setRan,setDom
                       ,filterDom,filterRan) where

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

data Relation a b = Relation {
  ranges :: M.Map a (S.Set b),
  domains :: M.Map b (S.Set a)
  }
                  deriving Show

empty = Relation M.empty M.empty
singleton a b = insert a b empty
fromList = foldr (uncurry insert) empty
toList r = [(a,b) | (a,bs) <- M.assocs (ranges r), b <- S.toList bs]

inverse (Relation a b) = Relation b a

insertVal a b m = M.alter (Just . S.insert b . fromMaybe S.empty) a m
deleteVal a b m = M.alter (mfilter (not . S.null) . fmap (S.delete b)) a m

insert a b (Relation ran dom) = Relation (insertVal a b ran) (insertVal b a dom)
delete a b (Relation ran dom) = Relation (deleteVal a b ran) (deleteVal b a dom)

member a b r = S.member b (lookupRan a r)
notMember a b = not . member a b

lookupRan a r = fromMaybe S.empty $ M.lookup a (ranges r)
range = S.unions . M.elems . ranges
lookupDom b = lookupRan b . inverse
domain = range . inverse

modifyRan f a r = r {
  ranges = M.alter (const $ if S.null newRan then Nothing else Just newRan) a (ranges r),
  domains = execState st (domains r)
  }
  where newRan = f oldRan ; oldRan = lookupRan a r
        st = sequence_ [modify (deleteVal b a) | b <- S.toList $ oldRan S.\\ newRan]
             >> sequence_ [modify (insertVal b a) | b <- S.toList $ newRan S.\\ oldRan]
modifyDom f b r = inverse (modifyRan f b (inverse r))

setRan a = flip modifyRan a . const
setDom b = flip modifyDom b . const

filterRan p r = inverse (filterDom p (inverse r))
filterDom p (Relation ran dom) = Relation (M.filterWithKey (const . p) ran) (M.map (S.filter p) dom)
