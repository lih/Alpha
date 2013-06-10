{-# LANGUAGE DeriveGeneric, StandaloneDeriving, NoMonomorphismRestriction #-}
module Serialize(module Data.Serialize) where

import Context as C
import Data.Bits
import qualified Data.Bimap as BM
import Data.Maybe
import Data.Monoid
import Data.Serialize
import GHC.Generics
import ID
import My.Control.Monad
import PCode

deriving instance Generic Builtin
deriving instance Generic C.Value
deriving instance Generic Axiom
deriving instance Generic (Range a)
instance Serialize Builtin
instance Serialize C.Value
instance Serialize Axiom
instance Serialize a => Serialize (Range a)

newtype SquashedI = SqI { unSqI :: Int }
newtype SquashedL a = SqL [a]
newtype SquashedM a = SqM (BM.Bimap String a)

instance Serialize SquashedI where
  put (SqI n) = putWord8 (fromIntegral $ length bytes) >> mapM_ putWord8 bytes
    where bytes = map fromIntegral $ takeWhile (>0) $ iterate (`shiftR`8) n
  get = getWord8 >>= \n -> do
    bytes <- mapM (const getWord8) [1..n]
    return $ SqI $ sum (zipWith shiftL (map fromIntegral bytes) [0,8..])
instance Serialize a => Serialize (SquashedL a) where
  put (SqL l) = put (SqI $ length l) >> mapM_ put l
  get = get >>= \(SqI n) -> SqL <$> forM [1..n] (const get)
instance (Ord a,Serialize a) => Serialize (SquashedM a) where
  put (SqM m) = put $ SqL [(SqL n,v) | (n,v) <- BM.toList m]
  get = get <§> \(SqL l) -> SqM (BM.fromList [(n,v) | (SqL n,v) <- l])

typeCodes = [(Value,0),(GValue,1),(Address,2),(Size,3),(SymID,4)]
instance Serialize PCode.Value where
  put (SymVal t n) = putWord8 (fromJust (lookup t typeCodes)) >> put n
  put (IntVal n) = putWord8 5 >> put n
  put NullVal = putWord8 6
  get = getWord8 >>= \n -> case lookup n (map (\(a,b) -> (b,a)) typeCodes) of
    Just t -> SymVal t <$> get
    Nothing -> case n of
      5 -> IntVal <$> get
      6 -> return NullVal
      _ -> error "Unrecognized Value prefix"
instance Serialize Code where
  put (Code [] [Branch _ []] Nothing) = putWord8 0
  put (Code [] instrs ret)            = putWord8 1 >> put (SqL instrs,ret)
  put (Code args instrs ret)          = putWord8 2 >> put (SqL args,SqL instrs,ret)
  get = getWord8 >>= \n -> case n of
    0 -> return $ Code [] [Branch NullVal []] Nothing
    1 -> get <§> \(SqL instrs,ret) -> Code [] instrs ret
    2 -> get <§> \(SqL args,SqL instrs,ret) -> Code args instrs ret
    _ -> error "Unrecognized Code prefix"

instance Serialize Instruction where
  put (Op b d [x])     = putWord8 0 >> put (b,d,x)
  put (Op b d [x,y])   = putWord8 1 >> put (b,d,x,y)
  put (Op b d l)       = putWord8 2 >> put (b,d,SqL l)
  put (Call d f l)     = putWord8 3 >> put (d,f,SqL l)
  put (Branch _ [])    = putWord8 4
  put (Branch _ [x])   = putWord8 5 >> put x
  put (Branch v [x,y]) = putWord8 6 >> put (v,x,y)
  put (Branch v l)     = putWord8 7 >> put (v,SqL (map SqI l))
  put (Bind bv x)      = putWord8 8 >> put (bv,x)
  put Noop             = putWord8 9
  get = getWord8 >>= \_x -> case _x of
    0 -> get <§> \(b,d,x) -> Op b d [x]
    1 -> get <§> \(b,d,x,y) -> Op b d [x,y]
    2 -> get <§> \(b,d,SqL l) -> Op b d l
    3 -> get <§> \(d,f,SqL l) -> Call d f l
    4 -> return $ Branch NullVal []
    5 -> get <§> \x -> Branch NullVal [x]
    6 -> get <§> \(v,x,y) -> Branch v [x,y]
    7 -> get <§> \(v,SqL l) -> Branch v (map unSqI l)
    8 -> get <§> \(bv,x) -> Bind bv x
    9 -> return Noop
    _ -> error "Unrecognized Instruction prefix"
instance Serialize BindVar where
  put (BindVar sym (0,1) 0 []) = putWord8 0 >> put sym
  put (BindVar sym (n,nr) pad []) = putWord8 1 >> put (sym,SqI n,SqI nr,SqI pad)
  put (BindVar sym (n,nr) pad subs) = putWord8 2 >> put (sym,SqI n,SqI nr,SqI pad,SqL subs)
  get = getWord8 >>= \_n -> case _n of
    0 -> get <§> \sym -> BindVar sym (0,1) 0 []
    1 -> get <§> \(sym,SqI n,SqI nr,SqI pad) -> BindVar sym (n,nr) pad []
    2 -> get <§> \(sym,SqI n,SqI nr,SqI pad,SqL subs) -> BindVar sym (n,nr) pad subs
    _ -> error "Unrecognized BindVar prefix"
instance Serialize ID where
  put (ID n) = put (SqI n)
  get = ID . unSqI <$> get

instance Serialize Language where
  put l = put (maxIDL l,SqM (symbolsL l),valuesL l,SqM (languagesL l))
  get = get <§> \(mi,SqM syms,vals,SqM langs) -> mempty{ maxIDL = mi, symbolsL = syms, valuesL = vals, languagesL = langs }
