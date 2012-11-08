{-# LANGUAGE ViewPatterns, TupleSections, ParallelListComp #-}
module Specialize.X86_64(arch_x86_64) where

import Control.Arrow
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Bits
import Data.Char
import Data.Function
import Data.Maybe
import Data.Monoid
import Data.Ord
import My.Control.Monad
import My.Control.Monad.State
import My.Data.Either
import My.Data.List
import My.Prelude
import Specialize.Types hiding (call)
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Bimap as BM
import qualified Data.Set as S
import qualified My.Data.SetBy as SB

fi           :: (Integral a,Num b) => a -> b
fis          :: (Integral a,Num b) => [a] -> [b]
bytes        :: (Bits a,Integral a,Num b) => a -> [b]
defSize      :: Num a => a
argBytesWide :: Bool -> Int -> Int -> (Maybe Integer) -> ([Word8],[Word8])
codeFun      :: [(Int,(Word8,Int))] -> Integer -> (Word8,Int,[Word8])

fis = fmap fromIntegral ; fi = fromIntegral
defSize = 8
bSize (bindSize -> (n,nr)) = n+nr*defSize
arch_x86_64 = Arch "x86_64" defSize defaults compile

fromFields fs = foldl1 xor (zipWith shiftL (map (fst) fs) (scanl (+) 0 $ map snd fs))

showHex n = reverse $ map (intToDigit . fromIntegral . (`mod`16)) $ take 2 $ iterate (`div`16) (n :: Word8)
showCode = intercalate " " . map showHex
bytes = fis . iterate (`shiftR`8)

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

([rax,rcx,rdx,rbx,rsp,rbp,rsi,rdi,r8,r9,r10,r11,r12,r13,r14,r15],allocRegs) =
  (regs,filter (not . (`elem`[rsp,r15])) regs)
  where regs = [0..15] :: [Int]

op code d a b | d==b = op d a
              | otherwise = mov d a++op d b
  where op d a = pre++code++suf
          where (pre,suf) = argBytes d a Nothing
opi codes d a n = mov d a ++ pre++[code]++suf++imm
  where (code,r,imm) = codes n
        (pre,suf) = argBytes r d Nothing
codeFun codes n = head [(code,r,take s divs) | (s,(code,r)) <- codes, s>=size]
  where size = length $ takeWhile (>0) divs
        divs = bytes n

numSize n = length $ takeWhile (>0) $ iterate (`shiftR`8) n
withSize n = (numSize n,return $ fi n)

mov d s | d==s = []
        | otherwise = pre++[0x8b]++suf
  where (pre,suf) = argBytes d s Nothing
movi d 0 = op [0x33] d d d
movi d n = pre++[code]++suf++imm
  where (code,r,imm) = codeFun [(4,(0xC7,0)),(8,(0xB8`xor`(fi d.&.7),0))] n
        (pre,suf) | code==0xC7 = argBytes 0 d Nothing
                  | otherwise = (fst $ argBytes d 0 Nothing,[])
lea d s n = pre++[0x8d]++post
  where (pre,post) = argBytes d s (Just n)

ld d (s,n,0) = []
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

calli v = [0xe8]++take 4 (bytes v)
call r = pre++[0xff]++post
  where (pre,post) = argBytes 2 r Nothing

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

fromBytes l = (length l,length l,return $ B.pack l)
fromBytesN n ml = (n,n,liftM B.pack ml)
(e,s,c) <++> (e',s',c') = (e+e',s+s',liftM2 B.append c c')
concatC = foldr1 (<++>)

infos = ask >>= \i -> lift (runtl get) >>= \(p,f) -> return (i,p,f)

withFreeSet m = infos >>= \(i,p,f) -> do
  let cmp r r' = case (regVar r,regVar r') of
        (Just _,Nothing) -> GT
        (Nothing,Just _) -> LT
        (Nothing,Nothing) -> case (fRegVar r,fRegVar r') of
          (Just _,Nothing) -> GT
          (Nothing,Just _) -> LT
          _ -> compare r r'
        (Just v,Just v') -> (isActive v`compare`isActive v')`mappend`compare r r'
      regVar r = BM.lookupR r $ registers p
      fRegVar r = BM.lookupR r $ fregisters f
      isActive v = S.member v $ actives i
  evalStateT m (SB.fromList cmp allocRegs)

pastState = lift . runtl . doF fstF
guardP p v = guard (p v) >> return v

allocReg sym = lift infos >>= \(_,_,f) -> do
  let st free = (reg,SB.delete reg free)
        where reg | SB.null free = r15
                  | otherwise = fromMaybe (SB.findMin free) $ BM.lookup sym (fregisters f) >>= guardP (`SB.member` free) 
  r <- state st
  lift $ pastState (modifyF registersF $ BM.insert sym r)
  return r
    
varSize i s = fromMaybe defSize $ M.lookup s (sizes i)
getFrameAddr s = lift $ runtl $ stateF (fstF <.> frameF) (withAddr defSize s)
        
storeRegs regs = lift infos >>= \(i,p,f) -> do
  let vars = [(r,s,M.lookup s $ bindings i) | r <- regs, s <- maybeToList $ BM.lookupR r (registers p)]
      groups = classesBy ((==)`on`parent) vars
      parent (_,_,b) = fmap fst b
      storeGroup g = do
        ~(reg,code) <- loadRoot $ parent $ head g
        sts <- concat $< mapM (store reg) g
        return $ code++sts
        where store base (r,s,b) = lift $ do
                n <- maybe (getFrameAddr s) return $ fmap snd b
                return $ st (base,fi n,fi $ varSize i s) r
      restrict m = do
        free <- get
        let ~(free',occ) = SB.partition isFree free
            isFree r = isNothing $ BM.lookupR r (registers p)
        put free'
        res <- m
        modify (SB.union occ)
        return res
  ret <- restrict $ concat $< mapM storeGroup groups
  lift $ pastState $ modifyF registersF $ \rs -> foldr BM.delete rs $ map snd3 vars
  return $ fromBytes ret

k = Kleisli

loadRoot (Just s) = lift infos >>= \(_,p,_) -> case BM.lookup s (registers p) of
  Just r -> return (r,[])
  Nothing -> allocReg s >>= \r -> lift (getFrameAddr s) >>= \a -> return (r,ld r (rsp,fi a,defSize))
loadRoot Nothing = return (rsp,[])

miscInfos info = (argVal,globVal,isLocal)
  where argVal (IntVal n) = Right $ withSize n
        argVal (SymVal Size s) = Right $ withSize $ fromMaybe defSize $ M.lookup s (sizes info)
        argVal (SymVal SymID (ID s)) = Right $ withSize $ s
        argVal (SymVal _ s) = maybe (Left s) (Right . (4,)) $ globVal s
        globVal s = if isLocal s then Nothing else Just (fi $< snd (envInfo info) s)
        isLocal s = S.member s (locals info)
                     
loadArgs args = lift infos >>= \(info,p,_) -> do
  let fixed = mapMaybe snd args
      argReserve (arg,Nothing) = runKleisli (left $ k f) (argVal arg)
        where f s = state st
                where st free = fromMaybe (Left s,free)
                                $ do r <- BM.lookup s (registers p)
                                     guard (SB.member r free)
                                     return (Right r,SB.delete r free)
      argReserve (_,Just r) = return (Left $ Right r)
      argAlloc = runKleisli (left $ k allocReg ||| k return)
      (argVal,globVal,isLocal) = miscInfos info
  lift2 $ modifyFuture $ \f -> f { fregisters = foldr (uncurry BM.insert) (fregisters f)
                                                      [(s,r) | (SymVal Value s,Just r) <- args] }
  mapM_ (modify . SB.delete) fixed
  allocs <- mapM argAlloc =<< mapM argReserve args
  let assocs = lefts [left (,arg,bind arg) all | all <- allocs | (arg,_) <- args]
      bind (SymVal _ s) | isLocal s = M.lookup s (bindings info)
                        | otherwise = Nothing
      groups = classesBy ((==)`on`parent) assocs
      parent (_,_,b) = fmap fst b
      loadGroup g = do
        ~(base,code) <- loadRoot $ parent $ head g
        lds <- concatC $< mapM (load base) g
        return $ fromBytes code <++> lds
        where load base (r,arg,b) = case argVal arg of
                Right (n,v) -> return $ fromBytesN (if n<=4 then 7 else 10) (movi r $< v)
                Left s -> liftM fromBytes $ lift $ do
                  n <- maybe (getFrameAddr s) return $ fmap snd b
                  let SymVal t _ = arg
                  return $ if t==Value then ld r (base,fi n,fi $ varSize info s) else lea r base (fi n)
              
  sts <- storeRegs $ lefts allocs
  lds <- concatC $< mapM loadGroup groups
  lift $ pastState (modifyF registersF $ \m -> foldr (uncurry BM.insert) m [(s,r) | ((SymVal _ s,_),Left r) <- zip args allocs])
  return (sts <++> lds,allocs)

defaults args ret = debug (Past pregs frame,Future fr)
  where (regArgs,stArgs) = partition ((<=defSize) . bSize) args
        (regs,nonRegs) = zipRest argRegs regArgs
        (retReg:funReg:argRegs) = allocRegs
        pregs = BM.fromList [(bindSym v,r) | (r,v) <- regs]
        frame = foldr (frameAlloc defSize) emptyFrame (stArgs++nonRegs)
        fr | bSize ret<=defSize = BM.singleton (bindSym ret) retReg
           | otherwise = BM.empty

compile (Op BCall d (fun:args)) = infos >>= \(i,p,f) -> withFreeSet $ do
  let (Past regs subFrame,_) = defaults [BindVar id (argSize arg,0) 0 [] | (id,arg) <- argAssocs] undefined
      argAssocs = zip (map ID [0..]) args
      argSize (SymVal Value s) = varSize i s
      argSize _ = defSize
      (argVal,_,_) = miscInfos i
  stRet <- storeRegs [rax]
  modify (SB.delete rax)
  ~(lds,f:_) <- loadArgs $ (fun,Nothing):[(arg,r) | (id,arg) <- argAssocs, let r = BM.lookup id regs, isJust r]
  sts <- get >>= storeRegs . SB.toList
  top <- lift2 $ getsp (frameTop . frame)
  let storeBig (id,arg) = lift $ case lookupAddr id subFrame of
        Just addr -> case argVal arg of
          Left s -> do
            let loadAddr (r,n) = getFrameAddr r >§ \a -> (ld r15 (rsp,fi a,defSize),r15,a)
            (code,base,n) <- maybe (liftM ([],rsp,) $ getFrameAddr s) loadAddr $ M.lookup s (bindings i)
            let addrs = [0,defSize..argSize arg]
            return $ fromBytes $ code++concat [ld rax (base,fi$n+a,fi sz)++st (rsp,fi$top+defSize+addr+a,fi sz) rax
                                              | a <- addrs, let sz = min defSize (argSize arg-a)]
          Right (s,v) -> return $ fromBytesN (if s<=4 then 7 else 10) (movi r15 $< v)
                         <++> fromBytes (st (rsp,fi$top+defSize+addr,defSize) rax)
        Nothing -> return $ fromBytes []
  bigSts <- concatC $< mapM storeBig argAssocs
  let callCode = fromBytes (addi rsp rsp $ fi top) <++> callc <++> fromBytes (subi rsp rsp $ fi top)
        where callc = case f of
                Right (_,v) -> fromBytesN 5 (calli $< v)
                Left r -> fromBytes $ call r
  return $ stRet <++> lds <++> sts <++> bigSts <++> callCode
compile (Op b d [a,a']) | isBinOp b = undefined
                        | otherwise = undefined
compile (Op b d args@(a:a':t)) | isBinOp b = liftM (foldl1 (<++>)) $ mapM compile $ Op b d [a,a']:[Op b d [SymVal Value d,a''] | a'' <- t]
                               | otherwise = undefined
compile (Branch v alts) = undefined
compile (Bind bv Nothing) = pastState $ modifyF frameF (frameAlloc defSize bv) >> return (fromBytes [])
compile Noop = return $ fromBytes []
