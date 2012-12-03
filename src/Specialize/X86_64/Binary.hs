{-# LANGUAGE NoMonomorphismRestriction, ViewPatterns #-}
module Specialize.X86_64.Binary where

import Control.Monad.Writer
import Data.Bits
import Data.Maybe
import Data.Ord
import Data.Word
import My.Data.List
import PCode.Instruction
import Specialize.Types
import qualified Data.ByteString as B

fi           :: (Integral a,Num b) => a -> b
fis          :: (Integral a,Num b) => [a] -> [b]
bytes        :: (Bits a,Integral a,Num b) => a -> [b]
defSize      :: Num a => a
argBytesWide :: Bool -> Int -> Int -> (Maybe Integer) -> ([Word8],[Word8])
codeFun      :: [(Int,(Word8,Int,Int))] -> (Int,IO Integer) -> Maybe (Word8,Int,Int,IO [Word8])

fi = fromIntegral ; fis = fmap fromIntegral

defSize = 8
registers@[rax,rcx,rdx,rbx,rsp,rbp,rsi,rdi,r8,r9,r10,r11,r12,r13,r14,r15] = [0..15] :: [Int]

(execStub,initStub, callStub0, callStub1) = (writerStub exec,writerStub init,
                                             writerStub . callStub0,writerStub . callStub1)
  where cStub loadArgs = do
          mapM_ push saved
          loadArgs
          call rdi
          mapM_ pop saved
          tellCode [0xc3]
        alphaStub loadArgs f = do
          loadArgs
          movi rsi (withSize (f :: Integer))
          call rsi
          tellCode [0xc3]
        saved = rbx:rbp:[r12..r15]
        init = cStub (mov rdx rsi)
        exec = cStub (return ())
        callStub0 = alphaStub (return ())
        callStub1 = alphaStub (mov rdi rdx)
        push r = tellCode $ pre++[0x50.|.(fi r.&.7)]
          where (pre,_) = argBytesWide False 0 r Nothing
        pop r = tellCode $ pre++[0x58.|.(fi r.&.7)]
          where (pre,_) = argBytesWide False 0 r Nothing
        writerStub stub = let BC (_,_,code) = execWriter stub in code

bSize (bindSize -> (n,nr)) = n+nr*defSize
numSize n | n>=0 = numSize 64 n
          | otherwise = 1+numSize 64 (-1-n)
  where numSize 0 _ = 1
        numSize bl n = case reverse $ takeWhile (>0) $ iterate (`shiftR`bl) n of
          [] -> 0
          (x:t) -> (length t)*bl + numSize (bl`div`2) x
withSize n = (numSize n,return $ fi n)
fromFields fs = foldl1 xor (zipWith shiftL (map (fst) fs) (scanl (+) 0 $ map snd fs))
bytes = fis . iterate (`shiftR`8)

fromBytesN n ml = BC (n,n,liftM B.pack ml)
fromBytes c = fromBytesN (length c) (return c)
tellCode c = tell $ fromBytes c

argBytes = argBytesWide True
argBytesWide w r rm arg = (fis pre,fis suf)
  where pre = if rex/=0x40 then [rex] else []
          where rex = fromFields [(rm`shiftR`3,1),(0,1),(r`shiftR`3,1),(fromEnum w,1),(4,4)]
        suf = [fromFields [(rm.&.7,3),(r.&.7,3),(mode,2)]] ++ sib ++ fis index
        (mode,index) = maybe (3,[]) fun arg
          where fun n | n == 0 = if rm.&.7==5 then (1,[0]) else (0,[])
                      | n <= 128 && n > -128 = (1,[n])
                      | otherwise = (2,take 4 $ bytes n)
        sib | mode/=3 && (rm.&.7 == 4) = [fromFields [(4,3),(4,3),(0,2)]]
            | otherwise = []

op code d a b | d==b = op d a
              | otherwise = mov d a >> op d b
  where op d a = tellCode $ pre++code++suf
          where (pre,suf) = argBytes d a Nothing
opi codes def d a n = case codes n of
  Just (code,r,s',imm) -> mov d a >> tell (fromBytesN (length pref+s') (liftM (pref++) imm))
    where (pre,suf) = argBytes r d Nothing
          pref = pre++[code]++suf
  Nothing -> movi rsi n >> op def d a rsi
codeFun codes (size,n) = listToMaybe [(code,r,count,imm count) | (s,(code,count,r)) <- codes, s>=size]
  where imm s = liftM (take s . bytes) n
                      
mov d s | d==s = return ()
        | otherwise = tellCode (pre++[0x8b]++suf)
  where (pre,suf) = argBytes d s Nothing
movi d (0,_) = bwxorrr d d d
movi d n = tell $ fromBytesN (length pref+s) (liftM (pref++) imm)
  where (code,r,s,imm) = fromJust $ codeFun [(31,(0xC7,4,0)),(64,(0xB8`xor`(fi d.&.7),8,0))] n
        (pre,suf) | code==0xC7 = argBytes 0 d Nothing
                  | otherwise = (fst $ argBytes d 0 Nothing,[])
        pref = pre++[code]++suf
lea d s n = tellCode $ pre++[0x8d]++post
  where (pre,post) = argBytes d s (Just n)
zxtnd r s = case (s :: Int) of
  1 -> tellCode (pre++[0x0f,0xb6]++post)
  2 -> tellCode (pre++[0x0f,0xb7]++post)
  _ -> shli r r sz >> shri r r sz
    where sz = withSize $ 8*(defSize-s)
  where (pre,post) = argBytes r r Nothing
sxtnd r s = case (s :: Int) of
  1 -> tellCode (pre++[0x0f,0xbe]++post)
  2 -> tellCode (pre++[0x0f,0xbf]++post)
  4 -> tellCode (pre++[0x63]++post)
  _ -> shli r r sz >> sari r r sz
    where sz = withSize $ 8*(defSize-s)
  where (pre,post) = argBytes r r Nothing
  
setcc r f = tellCode (pre++[0x0f,0x90.|.fi f]++post) >> zxtnd r 1
  where (pre,post) = argBytesWide False 0 r Nothing

shli = opi (codeFun [(8,(0xC1,1,4))]) undefined
shri = opi (codeFun [(8,(0xC1,1,5))]) undefined
sari = opi (codeFun [(8,(0xC1,1,7))]) undefined
rori d s n | n==0||n==64 = return ()
           | otherwise = opi (codeFun [(8,(0xC1,1,1))]) undefined d s (withSize n)
ld d (_,_,0) = return ()
ld d (s,n,size) = load
  where szs = maximumBy (comparing weight) $ permutations [sz | sz <- [8,4,2,1], sz.&.size /= 0]
          where weight l = sum $ zipWith f l $ sums l
                  where f s i = fromJust $ findIndex (\p -> m.&.p==0) $ iterate (`shiftR`1) s
                          where m = s-((n+i)`mod`s)
        load = sequence_ $ zipWith ldChunk (reverse $ zip (sums szs) szs) (True:repeat False)
        ldChunk (i,sz) fst = sh sz >> tellCode (pre'++pre++code++suf)
          where (pre,suf) = argBytesWide (sz==8) d s (Just (n+i))
                (pre',code) = fromJust (lookup sz [(8,([],[0x8b]))
                                                  ,(4,([],[0x8b]))
                                                  ,(2,([0x66],[0x8b]))
                                                  ,(1,([],[0x8a]))])
                sh sz | fst||sz==8 = return ()
                      | otherwise = shli d d (withSize (sz*8))
st (_,_,0) _ = return ()
st (d,n,size) s = store
  where szs = maximumBy (comparing weight) $ permutations [sz | sz <- [8,4,2,1], sz.&.size /= 0]
          where weight l = sum $ zipWith f l $ sums l
                  where f s i = fromJust $ findIndex (\p -> m.&.p==0) $ iterate (`shiftR`1) s
                          where m = s-((n+i)`mod`s)
        store = sequence_ $ reverse [stChunk a b | (a,b) <- zip (reverse $ zip (sums szs) szs) (True:repeat False)]
        stChunk (i,sz) lst = tellCode (pre'++pre++code++suf) >> sh sz
          where (pre,suf) = argBytesWide (sz==8) s d (Just (n+i))
                (pre',code) = fromJust (lookup sz [(8,([],[0x89]))
                                                  ,(4,([],[0x89]))
                                                  ,(2,([0x66],[0x89]))
                                                  ,(1,([],[0x88]))])
                sh sz | lst = rori s s ((8-i)*8)
                      | otherwise = rori s s (sz*8)

commOp c c' = (op c,opn,flip . opn)
  where opn = opi (codeFun c') c

addri d r (0,_) = return ()
addri d r v = addri' d r v
(addrr,addri',addir)      = commOp [0x03]      [(8,(0x83,1,0)),(32,(0x81,4,0))]
(mulrr,mulri,mulir)       = commOp [0x0F,0xAF] [(8,(0x6B,1,0)),(64,(0x69,8,0))]
(bwandrr,bwandri,bwandir) = commOp [0x23]      [(7,(0x83,1,4)),(31,(0x81,4,4))]
(bworrr,bworri,bworir)    = commOp [0x0b]      [(7,(0x83,1,1)),(31,(0x81,4,1))]
(bwxorrr,bwxorri,bwxorir) = commOp [0x33]      [(7,(0x83,1,6)),(31,(0x81,4,6))]
notr r = tellCode $ pre++[0xf7]++post
  where (pre,post) = argBytes 2 r Nothing
negr r = tellCode $ pre++[0xf7]++post
  where (pre,post) = argBytes 3 r Nothing
subrr d a b | d==b = op [0x2b] d d a >> negr d
            | otherwise = op [0x2b] d a b
subri d r (0,_) = return ()
subri d r v = opi (codeFun [(8,(0x83,1,5)),(32,(0x81,4,5))]) [0x2b] d r v
subir d n a | d==a = subri d d n >> negr d
            | otherwise = movi d n >> subrr d d a

cmprr _ a b = op [0x3b] a a b
cmpri _ a = opi (codeFun codes) [0x3b] a a
  where codes = [(8,(0x83,1,7)),(32,(0x81,4,7))]
cmpir _ n a = movi rsi n >> cmprr rsi rsi a

calli pos (size,v) = tell $ fromBytesN 5 $ do
  pos <- pos ; v <- v
  -- putStrLn $ "calli: size="++show size++" pos="++show pos++" dest="++show v
  return $ [0xe8]++take 4 (bytes (v-fi pos-5))
call r = tellCode $ pre++[0xff]++post
  where (pre,post) = argBytesWide False 2 r Nothing

opsCode (rr,ri,ir,ii) dest v v' = case (v,v') of
  (Left r,Left r') -> rr dest r r'
  (Left r,Right v) ->  ri dest r v
  (Right v,Left r) -> ir dest v r
  (Right (s,n),Right (s',n')) -> movi dest (min s s',liftM2 ii n n')

