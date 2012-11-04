{-# LANGUAGE ViewPatterns, TupleSections #-}
module Specialize.X86_64(arch_x86_64) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Bits
import Data.Char
import Data.Maybe
import Data.Ord
import My.Control.Monad.State
import My.Data.Either
import My.Data.List
import My.Prelude
import Specialize.Types
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Bimap as BM

defSize = 8
bSize (bindSize -> (n,nr)) = n+nr*defSize
arch_x86_64 = Arch "x86_64" defSize defaults compile

fromFields fs = foldl1 xor (zipWith shiftL (map (fromIntegral . fst) fs) (scanl (+) 0 $ map snd fs))

showHex n = reverse $ map (intToDigit . fromIntegral . (`mod`16)) $ take 2 $ iterate (`div`16) (n :: Word8)
showCode = intercalate " " . map showHex

argBytes :: Int -> Int -> Maybe Integer -> ([Word8],[Word8])
argBytes = argBytesWide True
argBytesWide w r rm arg = (pre,suf)
  where pre = if rex/=0x40 then [rex] else []
          where rex = fromFields [(rm`shiftR`3,1),(0,1),(r`shiftR`3,1),(fromEnum w,1),(4,4)]
        suf = [fromFields [(rm.&.7,3),(r.&.7,3),(mode,2)]] ++ sib ++ index
        (mode,index) = maybe (3,[]) fun arg
          where fun n | n == 0 = if rm.&.7==5 then (1,[0]) else (0,[])
                      | n <= 128 && n > -128 = (1,[fromIntegral n])
                      | otherwise = (2,take 4 $ map (fromIntegral . (.&.255)) $ iterate (`shiftR`8) n)
        sib | mode/=3 && (rm.&.7 == 4) = [fromFields [(4,3),(4,3),(0,2)]]
            | otherwise = []

([rax,rcx,rdx,rbx,rsp,rbp,rsi,rdi,r8,r9,r10,r11,r12,r13,r14,r15],allocRegs) =
  (regs,filter (not . (`elem`[rsp,r15])) regs)
  where regs = [0..15] :: [Int]

op code d a b | d==b = op d a
              | otherwise = mov d a++op d b
  where op d a = pre++code++suf
          where (pre,suf) = argBytes d a Nothing
opi codes d a n = mov d a ++ pre++[code]++suf++imm
  where (code,r,imm) = codes (n :: Integer)
        (pre,suf) = argBytes r d Nothing
codeFun codes n = head [(code,r,take s chop) | (s,(code,r)) <- codes, s>=size]
  where size = length $ takeWhile (>0) divs
        chop = map (.&.255) divs
        divs = map fromIntegral $ iterate (`shiftR`8) n

mov d s | d==s = []
        | otherwise = pre++[0x8B]++suf
  where (pre,suf) = argBytes d s Nothing
movi d 0 = op [0x33] d d d
movi d n = pre++[code]++suf++imm
  where (code,r,imm) = codeFun [(4,(0xC7,0)),(8,(0xB8`xor`(fromIntegral d.&.7),0))] n
        (pre,suf) | code==0xC7 = argBytes 0 d Nothing
                  | otherwise = (fst $ argBytes d 0 Nothing,[])
ld d (s,n,size) = load
  where szs = maximumBy (comparing weight) $ permutations [sz | sz <- [8,4,2,1], sz.&.size /= 0]
          where weight l = sum $ zipWith f l $ sums l
                  where f s i = fromJust $ findIndex (\p -> m.&.p==0) $ iterate (`shiftR`1) s
                          where m = s-((n+i)`mod`s)
        load = concat $ zipWith ldChunk (reverse $ zip (sums szs) szs) (True:repeat False)
        ldChunk (i,sz) fst = sh sz
                             ++ if sz==1 && d>=4 && d<8 then (if fst then [] else mov r15 d)++ld r15 (s,n+i,1)++mov d r15
                                else code++suf
          where (pre,suf) = argBytesWide (sz==8) d s (Just (n+i))
                code = pre++fromJust (lookup sz [(8,[0x8b]),(4,[0x8b]),(2,[0x66,0x8b]),(1,[0x8a])])
                sh sz | fst||sz==8 = []
                      | otherwise = shli d d (sz*8)

st (d,n,size) s = store
  where szs = maximumBy (comparing weight) $ permutations [sz | sz <- [8,4,2,1], sz.&.size /= 0]
          where weight l = sum $ zipWith f l $ sums l
                  where f s i = fromJust $ findIndex (\p -> m.&.p==0) $ iterate (`shiftR`1) s
                          where m = s-((n+i)`mod`s)
        store = concat $ reverse $ zipWith stChunk (reverse $ zip (sums szs) szs) (True:repeat False)
        stChunk (i,sz) lst = (if sz==1 && d>=4 && d<8 then mov r15 s++st (d,n+i,1) r15
                              else code++suf) ++ sh sz
          where (pre,suf) = argBytesWide (sz==8) s d (Just (n+i))
                code = pre++fromJust (lookup sz [(8,[0x89]),(4,[0x89]),(2,[0x66,0x89]),(1,[0x88])])
                sh sz | lst = rori s s ((8-i)*8)
                      | otherwise = rori s s (sz*8)

add = op [0x03]
sub = op [0x2B]
mul = op [0x0F,0xAF]
bwand = op [0x23]
bwor = op [0x0b]

addi = opi $ codeFun [(1,(0x83,0)),(4,(0x81,0))]
subi = opi $ codeFun [(1,(0x83,5)),(4,(0x81,5))]
bwandi = opi $ codeFun [(1,(0x83,4)),(4,(0x81,4))]
bwori = opi $ codeFun [(1,(0x83,1)),(4,(0x81,1))]
muli = opi $ codeFun [(1,(0x6B,0)),(8,(0x69,0))]
shli = opi $ codeFun [(1,(0xC1,4))]
shri = opi $ codeFun [(1,(0xC1,5))]
rori d s n | n==0||n==64 = []
           | otherwise = opi (codeFun [(1,(0xC1,1))]) d s n

(e,s,c) <++> (e',s',c') = (e+e',s+s',liftM2 B.append c c')

defaults args ret = debug (Past pregs addrs st,Future fr)
  where (regArgs,stArgs) = partition ((<=defSize) . bSize) args
        (regs,nonRegs) = zipRest (tail allocRegs) regArgs
        pregs = BM.fromList [(bindSym v,r) | (r,v) <- regs]
        sta = stArgs++nonRegs
        addrs = M.fromList $ zip (map bindSym sta) (sums $ map bSize sta)
        st = map (True,) $ map bSize sta
        fr | bSize ret<=defSize = M.singleton (bindSym ret) (head allocRegs)
           | otherwise = M.empty
                         
compile (Op BCall d (f:args)) = undefined
compile (Op b d [a,b]) | isBinOp b = undefined
                       | otherwise = undefined
compile (Op b d args@(a:b:t)) | isBinOp b = liftM (foldl1 (<++>)) $ mapM compile $ Op b d [a,b]:[Op b d [SymVal Value d,x] | x <- t]
                              | otherwise = undefined

