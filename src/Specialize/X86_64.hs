module Specialize.X86_64(arch_x86_64) where

import Specialize.Types
import Control.Monad
import qualified Data.ByteString as B
import Data.Bits
import Data.List
import Data.Char

import My.Prelude

defSize = 8
arch_x86_64 = Arch "x86_64" defSize defaults compile

fromFields fs = foldl1 xor (zipWith shiftL (map (fromIntegral . fst) fs) (scanl (+) 0 $ map snd fs))

showHex n = reverse $ map (intToDigit . fromIntegral . (`mod`16)) $ take 2 $ iterate (`div`16) (n :: Word8)  
showCode = intercalate " " . map showHex

argBytes :: Int -> Int -> (Maybe Integer) -> ([Word8],[Word8])
argBytes r rm arg = (pre,suf)
  where pre = [fromFields [(rm`shiftR`3,1),(0,1),(r`shiftR`3,1),(1,1),(4,4)]]
        suf = [fromFields [(rm.&.7,3),(r.&.7,3),(mode,2)]] ++ sib ++ index
        (mode,index) = maybe (3,[]) fun arg
          where fun n | n == 0 = if rm.&.7==5 then (1,[0]) else (0,[]) 
                      | n <= 128 && n > -128 = (1,[fromIntegral n])
                      | otherwise = (2,take 4 $ map (fromIntegral . (.&.255)) $ iterate (`shiftR`8) n)
        sib | mode/=3 && (rm.&.7 == 4) = [fromFields [(4,3),(4,3),(0,2)]]
            | otherwise = []
        
[rax,rbx,rsp,rbp,r12] = [0,3,4,5,12] :: [Int]

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
movi d n = pre++[code]++suf++imm
  where (code,r,imm) = codeFun [(4,(0xC7,0)),(8,(0xB8`xor`(fromIntegral d.&.7),0))] n
        (pre,suf) | code==0xC7 = argBytes 0 d Nothing
                  | otherwise = (fst $ argBytes d 0 Nothing,[])
ld d s n = pre++[0x8B]++suf
  where (pre,suf) = argBytes d s (Just n)

add = op [0x03]
sub = op [0x2B]
mul = op [0x0F,0xAF]
addi = opi $ codeFun [(1,(0x83,0)),(4,(0x81,0))]
subi = opi $ codeFun [(1,(0x83,5)),(4,(0x81,5))]
muli = opi $ codeFun [(1,(0x6B,0)),(8,(0x69,0))]

(e,s,c) <++> (e',s',c') = (e+e',s+s',liftM2 B.append c c')

defaults args ret = undefined
compile = undefined
