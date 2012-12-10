{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

module Serialize where

import Context as C
import Data.Bimap as BM
import Data.Maybe
import Data.Monoid
import Data.Serialize
import GHC.Generics
import ID
import My.Control.Monad
import PCode
import Translate
import qualified Data.Map as M
import qualified Data.Set as S

deriving instance Generic ValType
deriving instance Generic PCode.Value
deriving instance Generic Code
deriving instance Generic Instruction
deriving instance Generic BindVar
deriving instance Generic Builtin
deriving instance Generic C.Value
deriving instance Generic Axiom
deriving instance Generic ID
deriving instance Generic (Range a)

instance Serialize ValType
instance Serialize PCode.Value
instance Serialize Code
instance Serialize Instruction
instance Serialize BindVar
instance Serialize Builtin
instance Serialize C.Value
instance Serialize Axiom
instance Serialize ID
instance Serialize a => Serialize (Range a)
instance (Ord a,Ord b,Serialize a,Serialize b) => Serialize (Bimap a b) where
  get = BM.fromList $< get 
  put = put . BM.toList
  
set2Map s = M.fromAscList (zip (S.toAscList s) (repeat undefined))
instance Serialize Language where
  get = do
    mi <- get
    syms <- get
    langs <- get
    vals <- get
    return $ mempty { maxIDL = mi, symbolsL = syms, languagesL = langs, valuesL = vals }
  put l = do
    put (maxIDL l)
    put (BM.filter exportNameP (symbolsL l))
    put (languagesL l)
    put vals'
    where Language { exportsL = ex, equivsL = eqs } = l
          vals' = M.map (translate $ translateEquivs l) $ M.intersection (valuesL l) (set2Map ex)
          refs = S.fromList $ concatMap references (M.elems vals')
          exportNameP _ s = (S.member s ex || S.member s refs) && not (M.member s eqs)

        

