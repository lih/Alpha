{-# LANGUAGE NoMonomorphismRestriction, ViewPatterns, FlexibleContexts, ImplicitParams #-}
module Specialize.X86_64.Binary(
  -- * Architecture specifics
  regSize,registers,rax,rcx,rdx,rbx,rsp,rbp,rsi,rdi,r8,r9,r10,r11,r12,r13,r14,r15,
  valReg,addrReg,
  -- * Compatibility call stubs
  execStub,initStub,callStub0,callStub1,
  -- * Move and extension operations
  mkMov,mkZX,mkSX,mkLEA,mkSetCC,
  -- * Memory handling operations
  load,store,
  -- * Binary operations
  mkAdd,mkSub,mkMul,mkAnd,mkOr,mkXor,
  -- * Unary operations
  mkNeg,mkNot,
  -- * Control flow operations
  mkCall,mkRet,mkJCC,mkJmp,
  -- * Special operations
  mkCmp,mkDiv
  ) where

import My.Control.Monad
import My.Control.Monad.WriterAcc
import Data.Bits
import Data.Maybe
import Data.Ord
import Data.Word
import My.Data.List
import Specialize.BinCode
import Specialize.Info (absolute)
import qualified Data.ByteString as B
import Specialize.Memory (RegID,Location(..),Constant(..),applyC,toEnv)
import Misc (($^))

argBytesWide :: Bool -> RegID -> RegID -> (Maybe Integer) -> ([Word8],[Word8])
codeFun      :: [(Int,(Word8,Int,RegID))] -> Constant -> Maybe (Word8,RegID,Int,IO [Word8])
rax,rcx,rdx,rbx,rsp,rbp,rsi,rdi,r8,r9,r10,r11,r12,r13,r14,r15 :: RegID
execStub, initStub :: IO B.ByteString
callStub0, callStub1 :: Integer -> IO B.ByteString

regSize = 8
registers@[rax,rcx,rdx,rbx,rsp,rbp,rsi,rdi,r8,r9,r10,r11,r12,r13,r14,r15] = [0..15] :: [RegID]
valReg = rsi
addrReg = rdi
tellCode = tell . bc
binOpII _op = _op :: Integer -> Integer -> Integer
constOp _op d a b = mkMov d (Constant $ applyC (binOpII _op) a b)

(execStub,initStub, callStub0, callStub1) = (writerStub exec,writerStub _init,
                                             writerStub . _callStub0,writerStub . _callStub1)
  where cStub loadArgs = do
          mapM_ push saved
          loadArgs
          call rdi
          mapM_ pop saved
          tellCode [0xc3]
        alphaStub loadArgs f = do
          loadArgs
          mkMov rsi (Constant (AbsConst f))
          call rsi
          tellCode [0xc3]
        saved = rbx:rbp:[r12..r15]
        _init = cStub (mkMov rdx (Register rsi))
        exec = cStub (return ())
        _callStub0 = alphaStub (return ())
        _callStub1 = alphaStub (mkMov rdi (Register rdx))
        push r = tellCode $ pre++[0x50.|.(fi r.&.7)]
          where (pre,_) = argBytesWide False 0 r Nothing
        pop r = tellCode $ pre++[0x58.|.(fi r.&.7)]
          where (pre,_) = argBytesWide False 0 r Nothing
        writerStub = binCodeData . execWriter 

argBytes = argBytesWide True
argBytesWide w r rm arg = (pre,suf)
  where pre = if rex/=0x40 then [rex] else []
          where rex = fromFields [(rm`shiftR`3,1),(0,1),(r`shiftR`3,1),(fromEnum w,1),(4,4)]
        suf = [fromFields [(rm.&.7,3),(r.&.7,3),(mode,2)]] ++ sib ++ index
        (mode,index) = maybe (3,[]) fun arg
          where fun n | n == 0 = if rm.&.7==5 then (1,[0]) else (0,[])
                      | n <= 128 && n > -128 = (1,[fi n])
                      | otherwise = (2,take 4 $ bytes n)
        sib | mode/=3 && (rm.&.7 == 4) = [fromFields [(4,3),(4 :: Int,3),(0,2)]]
            | otherwise = []
codeFun codes val = listToMaybe [(code,r,cnt,imm cnt) | (s,(code,cnt,r)) <- codes, s>=size]
  where imm s = take s <$> bytes <$> n
        (size,n) = case val of EnvConst c -> (64,c) ; AbsConst c -> withSize c

mkMov d (Register s) | d==s = return ()
                     | otherwise = tellCode (pre++[0x8b]++suf)
  where (pre,suf) = argBytes d s Nothing
mkMov d (Constant (AbsConst 0)) = mkXor d (Register d) (Register d)
mkMov d (Constant c) = tellCode pref >> tell (bcM s imm)
  where (code,_,s,imm) = fromJust $ codeFun [(31,(0xC7,4,0)),(64,(0xB8`xor`(fi d.&.7),8,0))] c
        (pre,suf) | code==0xC7 = argBytes 0 d Nothing
                  | otherwise = (fst $ argBytes d 0 Nothing,[])
        pref = pre++[code]++suf
mkMov _ _ = error "Cannot generate 'mov' instruction from other than a constant or a register"
mkZX r s = case (s :: Int) of
  1 -> tellCode (pre++[0x0f,0xb6]++post)
  2 -> tellCode (pre++[0x0f,0xb7]++post)
  _ -> shli r r sz >> shri r r sz
    where sz = AbsConst (fi $ 8*regSize-s)
  where (pre,post) = argBytes r r Nothing
mkSX r s = case (s :: Int) of
  1 -> tellCode (pre++[0x0f,0xbe]++post)
  2 -> tellCode (pre++[0x0f,0xbf]++post)
  4 -> tellCode (pre++[0x63]++post)
  _ -> shli r r sz >> sari r r sz
    where sz = AbsConst (fi $ 8*regSize-s)
  where (pre,post) = argBytes r r Nothing
mkLEA d (Constant c) n = constOp (+) d c (AbsConst $ fi n)
mkLEA d (Register s) n = tellCode $ pre++[0x8d]++post
  where (pre,post) = argBytes d s (Just n)
mkLEA _ _ _ = error "Cannot generate 'lea' instruction from other than a constant or a register"

mkSetCC r f = tellCode (pre++[0x0f,0x90.|.fi f]++post) >> mkZX r 1
  where (pre,post) = argBytesWide False 0 r Nothing

operator rr ri ir ii _a _b = case (_a,_b) of
  (Register a,Register b) -> rr a b
  (Register a,Constant b) -> ri a b
  (Constant a,Register b) -> ir a b
  (Constant a,Constant b) -> ii a b
  _ -> error "Cannot generate an operator from other than a constants or registers"
commutative rr ri ii = operator rr ri (flip ri) ii
op code d a b | d==b = mk d a
              | otherwise = mkMov d (Register a) >> mk d b
  where mk d' a' = tellCode (pre++code++suf)
          where (pre,suf) = argBytes d' a' Nothing
opi codes def d a n = case codes n of
  Just (code,r,s',imm) -> mkMov d (Register a) >> tellCode pref >> tell (bcM s' imm)
    where (pre,suf) = argBytes r d Nothing
          pref = pre++[code]++suf
  Nothing -> mkMov valReg (Constant n) >> op def d a valReg

mkNot d s = mkMov d s >> tellCode (pre++[0xf7]++post)
  where (pre,post) = argBytes 2 d Nothing
mkNeg d s = mkMov d s >> tellCode (pre++[0xf7]++post)
  where (pre,post) = argBytes 3 d Nothing
mkDiv (Constant n) = mkMov valReg (Constant n) >> mkDiv (Register valReg)
mkDiv (Register r) = tellCode $ pre++[0xf7]++post
  where (pre,post) = argBytes 7 r Nothing
mkDiv _ = error "Cannot generate 'div' instruction from other than a constant or register"

mkAdd d = commutative (op add d) (opi addi add d) (constOp (+) d)
  where add = [0x03] ; addi = codeFun [(8,(0x83,1,0)),(32,(0x81,4,0))]
mkSub d = operator subrr subri subir (constOp (-) d)
  where subrr a b | d==b = op [0x2b] d d a >> mkNeg d (Register d)
                  | otherwise = op [0x2b] d a b
        subri = withSpecial ignoreZero (opi (codeFun [(8,(0x83,1,5)),(32,(0x81,4,5))]) [0x2b]) d
        subir n a | d==a = subri d n >> mkNeg d (Register d)
                  | otherwise = mkMov d (Constant n) >> subrr d a
mkMul d = commutative (op mul d) mulri (constOp (*) d)
  where mul = [0x03]
        mulri a i = case codeFun [(8,(0x6B,1,undefined)),(32,(0x69,4,undefined))] i of
          Just (code,_,sz,imm) -> tellCode pref >> tell (bcM sz imm)
            where (pre,suf) = argBytes d a Nothing ; pref = pre++[code]++suf
          Nothing -> mkMov valReg (Constant i) >> mkMul d (Register a) (Register valReg)
mkAnd d = commutative (op _and d) (opi andi _and d) (constOp (.&.) d)
  where _and = [0x23] ; andi = codeFun [(7,(0x83,1,4)),(31,(0x81,4,4))]
mkOr d = commutative (op _or d) (opi ori _or d) (constOp (.|.) d)
  where _or = [0x0b] ; ori = codeFun [(7,(0x83,1,1)),(31,(0x81,4,1))]
mkXor d = commutative (op x d) (opi xi x d) (constOp xor d)
  where x = [0x33] ; xi = codeFun [(7,(0x83,1,6)),(31,(0x81,4,6))]

mkCmp = operator cmprr cmpri cmpir (error $ "Cannot compare two constants")
  where cmprr a b = op [0x3b] 0 a b
        cmpri a = opi (codeFun codes) [0x3b] 0 a
          where codes = [(8,(0x83,1,7)),(32,(0x81,4,7))]
        cmpir n a = mkMov valReg (Constant n) >> cmprr valReg a

call r = tellCode $ pre++[0xff]++post
  where (pre,post) = argBytesWide False 2 r Nothing
mkCall (Register r) = call r
mkCall (Constant c) = void $ mfix $ \pos -> do
  tellCode [0xe8]
  tell $ bcM 4 $ absolute (posAddr pos) <**> toEnv c <§> \d s -> take 4 (bytes (d-s))
  getAcc
mkCall _ = error "Cannot generate 'call' instruction from other than a constant or register"
mkRet = tellCode [0xc3]
mkJCC short long (Position (Sum e',Sum s')) = void $ mfix $ \(Position (Sum e,Sum s)) -> do
  let de = e'-e ; ds = s'-s
      code | de==0 = []
           | de > -128 && de<=128 = short++take 1 (bytes ds)
           | otherwise = long++take 4 (bytes ds)
  tell $ BC (length long+4,length code,return $ B.pack code)
  getAcc
mkJmp = mkJCC [0xeb] [0xe9]

withSpecial spe f d r (AbsConst n) = fromMaybe (f d r (AbsConst n)) $ spe d r n
withSpecial _ f d r v = f d r v
ignoreZero _ _ = lookup $^ [(0,return ())]

shli = withSpecial ignoreZero $ opi (codeFun [(8,(0xC1,1,4))]) undefined
shri = withSpecial ignoreZero $ opi (codeFun [(8,(0xC1,1,5))]) undefined
sari = withSpecial ignoreZero $ opi (codeFun [(8,(0xC1,1,7))]) undefined
rori d s n | n==0||n==64 = return ()
           | otherwise = opi (codeFun [(8,(0xC1,1,1))]) undefined d s (AbsConst n)
load _ (_,_,0) = return ()
load d (src,n,size) = sequence_ $ zipWith ldChunk (reverse $ zip (sums szs) szs) (True:repeat False)
  where szs = maximumBy (comparing weight) $ permutations [sz | sz <- [8,4,2,1], sz.&.size /= 0]
          where weight l = sum $ zipWith f l $ sums l
                  where f s i = fromJust $ findIndex (\p -> m.&.p==0) $ iterate (`shiftR`1) s
                          where m = s-((n+i)`mod`s)
        ldChunk (i,sz) first = sh >> tellCode (pre'++pre++code++suf)
          where (pre,suf) = argBytesWide (sz==8) d src (Just (n+i))
                (pre',code) = fromJust (lookup sz [(8,([],[0x8b]))
                                                  ,(4,([],[0x8b]))
                                                  ,(2,([0x66],[0x8b]))
                                                  ,(1,([],[0x8a]))])
                sh | first||sz==8 = return ()
                   | otherwise = shli d d (AbsConst (sz*8))
store (_,_,0) _ = return ()
store (d,n,size) src = sequence_ $ reverse [stChunk a b | (a,b) <- zip (reverse $ zip (sums szs) szs) (True:repeat False)]
  where szs = maximumBy (comparing weight) $ permutations [sz | sz <- [8,4,2,1], sz.&.size /= 0]
          where weight l = sum $ zipWith f l $ sums l
                  where f s i = fromJust $ findIndex (\p -> m.&.p==0) $ iterate (`shiftR`1) s
                          where m = s-((n+i)`mod`s)
        stChunk (i,sz) lst = tellCode (pre'++pre++code++suf) >> sh 
          where (pre,suf) = argBytesWide (sz==8) src d (Just (n+i))
                (pre',code) = fromJust (lookup sz [(8,([],[0x89]))
                                                  ,(4,([],[0x89]))
                                                  ,(2,([0x66],[0x89]))
                                                  ,(1,([],[0x88]))])
                sh | lst = rori src src ((8-i)*8)
                   | otherwise = rori src src (sz*8)

