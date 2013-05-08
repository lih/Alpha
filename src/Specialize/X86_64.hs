{-# LANGUAGE ImplicitParams, NoMonomorphismRestriction, PatternGuards, TupleSections  #-}
module Specialize.X86_64(arch,execStub,initStub,callStub0,callStub1) where

import PCode
import Data.Maybe
import Data.Either
import Data.Function (on)
import Control.Monad.State.View hiding (on)
import My.Control.Monad
import My.Control.Monad.Future
import Specialize.Info
import Specialize.Memory
import Specialize.Architecture
import qualified My.Data.Relation as R
import My.Data.Relation ((<!-),(-!-),(-!>),(-?-))
import Specialize.X86_64.Binary
import qualified Data.Set as S
import My.Data.List
import Specialize.Frame
import ID
import Misc (($^))

arch = Arch "x86-64" regSize initials compile
allocRegs = [r | r <- registers, r<rsp || r>=r8]

initials args ret = let ?defaultSize = regSize in (MemState plocs _frame,FutureState fr)
  where (regArgs,stArgs) = partition (bSize >>> (<=regSize)) args
        (regs,nonRegs) = zipRest argRegs regArgs
        (retReg:_:argRegs) = allocRegs
        plocs = R.fromList $ [(bindSym v,Register r) | (r,v) <- regs]
                ++ [(bindSym v,Memory) | v <- stArgs++nonRegs]
        _frame = foldr (frameAlloc regSize) emptyFrame (stArgs++nonRegs)
        fr = case ret of
          Just ret' | bSize ret'<=regSize -> R.singleton (bindSym ret') (Register retReg)
          _ -> R.empty

compile i = let ?defaultSize = regSize in do
  changeFuture i
  saveFuture $ do
--    unless (isBranch i) storeFlags
    compile' i

-- storeFlags = error "Undefined storeFlags"

changeFuture (Op op _ [a,_]) | op`elem`[BMod,BDiv] = future $ modifying flocations_ $ (argVar a <!- Register rax) . (S.empty <!- Register rdx)
  where argVar (SymVal Value s) = S.singleton s ; argVar _ = S.empty
changeFuture _ = return ()

compile' (Op b d [x,y]) | b`elem`[BMod,BDiv] = do
  [yl] <- loadArgs [(x,Register rax),(IntVal 0,Register rdx)] [y]
  mkDiv yl
  modifying locations_ $ d -!- Register (case b of BDiv -> rax; BMod -> rdx; _ -> undefined)
compile' (Op b d [x,y]) | isCompare b = do
  [xl,yl] <- loadArgs [] [x,y]
  mkCmp xl yl
  modifying locations_ $ d -!- Flags (flag b)
  where flag = error "Undefined flag"
compile' (Op b d [x,y]) | isBinOp b = do
  [xl,yl] <- loadArgs [] [x,y]
  dr <- destRegister d
  mkBinOp b dr xl yl
  modifying locations_ $ d -!- Register dr
  where mkBinOp = error "Undefined mkBinOp"

compile' (Op b d [s]) | Just op <- lookup b [(BSub,mkNeg),(BNot,mkNot)] = do
  [sl] <- loadArgs [] [s]
  dr <- destRegister
  op dr sl
  modifying locations_ $ d -!- Register dr
compile' (Op b d [s]) | b`elem`[BSet,BSetSX] = do
  [sl] <- loadArgs [] [s]
  case sl of
    Register _ | varSize d==argSize s -> modifying locations_ (d -!> S.singleton sl)
    _ -> do
      dr <- destRegister d
      mkMov dr sl
      when (argSize s < varSize d) $ case b of
        BSet -> mkZX dr (argSize s)
        BSetSX -> mkSX dr (argSize s)
        _ -> error "Impossible branch"

compile' (Branch _ []) = mkRet

compile' _ = return ()

loadArgs fixed new = do
  p <- get
  let locs = locations p ; domOf = R.lookupDom $^ locs ; isFree = S.null . domOf
      allocList = sortByNat (S.size . domOf . Register) allocRegs
      fixedRegs = S.fromList [r | (_,Register r) <- fixed]
      allocReg = state (\_rs -> case _rs of [] -> (Register addrReg,[]) ; (r:rs) -> (Register r,rs))
      allocVal (SymVal t s) | t==Value || t==Address = allocReg
      allocVal v = return $ Constant $ case v of
        SymVal GValue s -> EnvConst (globAddress s)
        SymVal Size s -> AbsConst (fromIntegral $ varSize s)
        SymVal SymID (ID s) -> AbsConst (fromIntegral s)
        IntVal n -> AbsConst n
  evalStateT $^ allocList $ do
    modify (filter (not . (S.member $^ fixedRegs)))
    ret <- mapM allocVal new
    let allocated = fixed ++ zip new ret
        overwritten = S.fromList [r | (_,Register r) <- allocated]
                      S.\\ S.fromList [r | (SymVal Value s,v@(Register r)) <- allocated, s -?- v $ locs]
    lift $ viewing locations_ $ forM (S.toList overwritten) $ \r -> modify (S.empty <!- Register r)
    locs' <- lift $ gets locations 
    let saved = [SymVal Value s | r <- tl overwritten, s <- tl (locs R.>?- Register r)
                                , not (S.null (locs' R.-?< s))]
        allAssocs = map (,Memory) saved ++ allocated
        groupAllocs l = (inSt,inMem)
          where (inSt,grs) = partitionEithers (map f l)
                f (s,i) | Just (r,n) <- binding s = Right (r,(s,n,i))
                        | otherwise = Left (s,i)
                inMem = [(fst (head g),map snd g) | g <- classesBy ((==) `on` fst) grs]
        
        mems = groupAllocs [(s,()) | (SymVal Value s,Memory) <- allAssocs]
        
    modify (takeWhile (isFree . Register))
    return ret
  where tl = S.toList
  
destRegister = error "Undefined destRegister"
