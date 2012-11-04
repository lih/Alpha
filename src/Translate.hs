module Translate where

import My.Control.Monad
import Data.Bimap as BM
import ID
import PCode
import Context.Types as C

class Translatable t where
  translate :: (ID -> ID) -> t -> t

instance Translatable ID where
  translate = ($)
instance Translatable a => Translatable (Range a) where
  translate t (Range (a,b)) = Range (translate t a,translate t b)
instance Translatable e => Translatable [e] where 
  translate tr l = map (translate tr) l
instance Translatable C.Value where 
  translate tr (Verb code) = Verb (translate tr code)
  translate tr (Noun size init) = Noun (translate tr size) (translate tr init)
  translate tr x = x
instance Translatable Instruction where
  translate tr (Op b v vs) = Op b (translate tr v) (translate tr vs)
  translate tr (Branch v as) = Branch (translate tr v) as
  translate tr (Bind bv v) = Bind (translate tr bv) (fmap (translate tr) v)
  translate tr Noop = Noop
instance Translatable PCode.Value where
  translate tr (SymVal t id) = SymVal t (translate tr id)
  translate tr v = v
instance Translatable Code where
  translate tr (Code args code ret) = Code (translate tr args) (translate tr code) (translate tr ret)
instance Translatable BindVar where
  translate tr (BindVar id s pad subs) = BindVar (translate tr id) s pad [(translate tr bv,s) | (bv,s) <- subs]

