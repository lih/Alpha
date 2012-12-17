{-# LANGUAGE DeriveGeneric, StandaloneDeriving, NoMonomorphismRestriction #-}
module Serialize() where

import Context as C
import Data.Bits
import qualified Data.Bimap as BM
import Data.Maybe
import Data.Monoid
import Data.Serialize
import Data.Word
import GHC.Generics
import ID
import My.Control.Monad
import PCode

deriving instance Generic Code
deriving instance Generic BindVar
deriving instance Generic Builtin
deriving instance Generic C.Value
deriving instance Generic Axiom
deriving instance Generic (Range a)

instance Serialize a => Serialize (Range a)
instance (Ord a,Ord b,Serialize a,Serialize b) => Serialize (BM.Bimap a b) where
  get = BM.fromList $< get 
  put = put . BM.toList

newtype Squashed = Squash { unSquash :: Int }

instance Serialize Squashed where
  put (Squash n) = do
    let bytes = map fromIntegral $ takeWhile (>0) $ iterate (`shiftR`8) n
    putWord8 (fromIntegral $ length bytes)
    mapM_ putWord8 bytes
  get = do
    n <- getWord8
    bytes <- mapM (const getWord8) [1..n]
    return $ Squash $ sum (zipWith shiftL (map fromIntegral bytes) [0,8..])

instance Serialize Builtin
instance Serialize C.Value
instance Serialize Axiom
typeCodes = [(Value,0),(GValue,1),(Address,2),(Size,3),(SymID,4)]
instance Serialize PCode.Value where
  put (SymVal t n) = putWord8 (fromJust (lookup t typeCodes)) >> put n
  put (IntVal n) = putWord8 5 >> put n
  put NullVal = putWord8 6
  get = getWord8 >>= \n -> case lookup n (map (\(a,b) -> (b,a)) typeCodes) of
    Just t -> SymVal t $< get
    Nothing -> case n of
      5 -> IntVal $< get
      6 -> return NullVal
instance Serialize Code where
  put (Code [] [Branch _ []] Nothing) = putWord8 0
  put (Code [] instrs ret)            = putWord8 1 >> put (instrs,ret)
  put (Code args instrs ret)          = putWord8 2 >> put (args,instrs,ret)
  get = getWord8 >>= \n -> case n of
    0 -> return $ Code [] [Branch NullVal []] Nothing
    1 -> get >§ \(instrs,ret) -> Code [] instrs ret
    2 -> get >§ \(args,instrs,ret) -> Code args instrs ret

instance Serialize Instruction where
  put (Op b d [x])     = putWord8 0 >> put (b,d,x)
  put (Op b d [x,y])   = putWord8 1 >> put (b,d,x,y)
  put (Op b d l)       = putWord8 2 >> put (b,d,l)
  put (Branch _ [])    = putWord8 3
  put (Branch _ [x])   = putWord8 4 >> put x
  put (Branch v [x,y]) = putWord8 5 >> put (v,x,y)
  put (Branch v l)     = putWord8 6 >> put (v,l)
  put (Bind bv x)      = putWord8 7 >> put (bv,x)
  put Noop             = putWord8 8
  get = getWord8 >>= \x -> case x of
    0 -> get >§ \(b,d,x) -> Op b d [x]
    1 -> get >§ \(b,d,x,y) -> Op b d [x,y]
    2 -> get >§ \(b,d,l) -> Op b d l
    3 -> return $ Branch NullVal []
    4 -> get >§ \x -> Branch NullVal [x]
    5 -> get >§ \(v,x,y) -> Branch v [x,y]
    6 -> get >§ \(v,l) -> Branch v l
    7 -> get >§ \(bv,x) -> Bind bv x
    8 -> return Noop
instance Serialize BindVar where
  put (BindVar sym (0,1) 0 []) = putWord8 0 >> put sym
  put (BindVar sym (n,nr) pad []) = putWord8 1 >> put (sym,Squash n,Squash nr,Squash pad)
  put (BindVar sym (n,nr) pad subs) = putWord8 2 >> put (sym,Squash n,Squash nr,Squash pad,subs)
  get = getWord8 >>= \n -> case n of
    0 -> get >§ \sym -> BindVar sym (0,1) 0 []
    1 -> get >§ \(sym,Squash n,Squash nr,Squash pad) -> BindVar sym (n,nr) pad []
    2 -> get >§ \(sym,Squash n,Squash nr,Squash pad,subs) -> BindVar sym (n,nr) pad subs
instance Serialize ID where
  put (ID n) = put (Squash n)
  get = ID . unSquash $< get
  
instance Serialize Language where
  put l = put (maxIDL l,symbolsL l,valuesL l,languagesL l)
  get = get >§ \(mi,syms,vals,langs) -> mempty{ maxIDL = mi, symbolsL = syms, valuesL = vals, languagesL = langs }
