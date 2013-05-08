module Translate where

import qualified Data.Map as M
import Data.Maybe
import ID
import PCode
import Context.Types as C

class Translatable t where
  translate :: (ID -> ID) -> t -> t

mapTranslate m = \s -> fromMaybe s $ M.lookup s m

instance Translatable ID where
  translate = ($)
instance Translatable a => Translatable (Range a) where
  translate t (Range (a,b)) = Range (translate t a,translate t b)
instance Translatable e => Translatable [e] where 
  translate tr l = map (translate tr) l
instance Translatable C.Value where 
  translate tr (Verb code) = Verb (translate tr code)
  translate tr (Noun sz i) = Noun (translate tr sz) (translate tr i)
  translate _ x = x
instance Translatable Instruction where
  translate tr (Op b v vs) = Op b (translate tr v) (translate tr vs)
  translate tr (Call v f vs) = Call (translate tr v) (translate tr f) (translate tr vs)
  translate tr (Branch v as) = Branch (translate tr v) as
  translate tr (Bind bv v) = Bind (translate tr bv) (fmap (translate tr) v)
  translate _ Noop = Noop
instance Translatable PCode.Value where
  translate tr (SymVal t sym) = SymVal t (translate tr sym)
  translate _ v = v
instance Translatable Code where
  translate tr (Code args code ret) = Code (translate tr args) (translate tr code) (fmap (translate tr) ret)
instance Translatable BindVar where
  translate tr (BindVar sym s pad subs) = BindVar (translate tr sym) s pad [(translate tr bv,s') | (bv,s') <- subs]

