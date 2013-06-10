module Syntax where

import Prelude hiding (foldl,concatMap,elem)
import Data.List (intercalate)
import Data.Foldable
import Data.Traversable
import Control.Applicative

data Syntax a = Group [Syntax a]
              | Symbol a
              deriving Eq
               
instance Functor Syntax where
  fmap f (Symbol s) = Symbol $ f s
  fmap f (Group l) = Group $ map (fmap f) l
instance Foldable Syntax where
  foldl f e (Symbol s) = f e s
  foldl f e (Group l) = foldl (foldl f) e l
instance Traversable Syntax where
  traverse f (Symbol s) = Symbol <$> f s
  traverse f (Group l) = Group <$> traverse (traverse f) l 

instance Show a => Show (Syntax a) where
  show (Symbol s) = show s
  show (Group l) = "[" ++ intercalate " " (map show l) ++ "]"

newtype SynStr = SynStr String
instance Show SynStr where
  show (SynStr s) = concatMap trans s
    where trans c | c`elem`".:,;\\\n \t(){}[]" = '\\':[c]
                  | otherwise = [c]
