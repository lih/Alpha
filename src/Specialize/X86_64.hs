{-# LANGUAGE ViewPatterns, TupleSections, ParallelListComp, ImplicitParams #-}
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
opSize codes d a sz = length $ opi (codeFunS codes) d a 0
  where codeFunS codes = const $ head [(code,r,replicate sz 0) | (s,(code,r)) <- codes, s>=sz]
                      
numSize n = length $ takeWhile (>0) $ iterate (`shiftR`8) n

mov d s | d==s = []
        | otherwise = pre++[0x8b]++suf
  where (pre,suf) = argBytes d s Nothing
movi d 0 = bwxor d d d
movi d n = pre++[code]++suf++imm
  where (code,r,imm) = codeFun [(4,(0xC7,0)),(8,(0xB8`xor`(fi d.&.7),0))] n
        (pre,suf) | code==0xC7 = argBytes 0 d Nothing
                  | otherwise = (fst $ argBytes d 0 Nothing,[])
lea d s n = pre++[0x8d]++post
  where (pre,post) = argBytes d s (Just n)

shli = opi $ codeFun [(1,(0xC1,4))]
shri = opi $ codeFun [(1,(0xC1,5))]
rori d s n | n==0||n==64 = []
           | otherwise = opi (codeFun [(1,(0xC1,1))]) d s n
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

commOp c c' f = (op c,opn,flip . opn,f :: Integer -> Integer -> Integer,opSize c')
  where opn = opi (codeFun c')

adds@(add,addi,_,_,_) = commOp [0x03] [(1,(0x83,0)),(4,(0x81,0))] (+)
muls@(mul,muli,_,_,_) = commOp [0x0F,0xAF] [(1,(0x6B,0)),(8,(0x69,0))] (*)
bwands@(bwand,bwandi,_,_,_) = commOp [0x23] [(1,(0x83,4)),(4,(0x81,4))] (.&.)
bwors@(bwor,bwori,_,_,_) = commOp [0x0b] [(1,(0x83,1)),(4,(0x81,1))] (.|.)
bwxors@(bwxor,bwxori,_,_,_) = commOp [0x33] [(1,(0x83,6)),(4,(0x81,6))] xor
subs@(sub,subi,_,_,_) = (sub,opi $ codeFun codes,subi',(-),opSize codes)
  where neg r = pre++[0xf7]++post
          where (pre,post) = argBytes 3 r Nothing
        sub d a b | d==b = op [0x2b] d d a++neg d
                  | otherwise = op [0x2b] d a b
        codes = [(1,(0x83,5)),(4,(0x81,5))]
        subi' d n a | d==a = subi d d n++neg d
                    | otherwise = movi d n++sub d d a

calli v = [0xe8]++take 4 (bytes v)
call r = pre++[0xff]++post
  where (pre,post) = argBytes 2 r Nothing

fromBytesN n ml = BC (n,n,liftM B.pack ml)
fromBytes c = fromBytesN (length c) (return c)
tellCode c = tell $ fromBytes c

infos = liftM3 (,,) ask get (future get)
argSize (SymVal Value s) = varSize s
argSize _ = return defSize
verify p m = m >>= \v -> guard (p v) >> return v
k = Kleisli
withSize n = (numSize n,return $ fi n)

miscInfos = ask >>= \info -> do
  let argVal (IntVal n) = Right $ withSize n
      argVal (SymVal Size s) = Right $ withSize $ fromMaybe defSize $ M.lookup s (sizes info)
      argVal (SymVal SymID (ID s)) = Right $ withSize $ s
      argVal (SymVal _ s) = maybe (Left s) (Right . (4,)) $ globVal s
      globVal s = if isLocal s then Nothing else Just (fi $< snd (envInfo info) s)
      isLocal s = S.member s (locals info)
      binding s = M.lookup s (bindings info)
  return (argVal,globVal,isLocal,binding)

lookupRegSym = BM.lookupR
lookupSymReg = BM.lookup

getDestReg d = infos >§ \(i,p,f) -> case lookupSymReg d (fregisters f) of
  Just r -> r
  Nothing -> fst $ minimumBy (comparing snd) [(r,isJust $ verify (`S.member`actives i)
                                                 $ lookupRegSym r (registers p))
                                             | r <- allocRegs]
varSize s = fromMaybe defSize $< asks (M.lookup s . sizes) 
frameAddr s = stateF frameF (withAddr defSize s)

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

allocReg sym = lift (future $ gets fregisters) >>= \regs -> do
  let st free = (r,SB.delete r free)
        where r | SB.null free = r15
                | otherwise = fromMaybe (SB.findMin free) $ verify (`SB.member`free) $ lookupSymReg sym regs 
  r <- state st
  lift $ modifyF registersF $ BM.insert sym r
  return r
    
noFuture m = StateT s
  where s t = RWTL (\r (p,f) -> let (p,_,a,w) = runRWTL (runStateT m t) r (p,f) in (p,f,a,w))

loadRoot (Just s) = lift get >>= \p -> case lookupSymReg s (registers p) of
  Just r -> return r
  Nothing -> do
    r <- allocReg s
    a <- lift (frameAddr s)
    tellCode $ ld r (rsp,fi a,defSize) 
    return r
loadRoot Nothing = return rsp

storeRegs regs = noFuture $ lift infos >>= \(i,p,f) -> do
  let vars = [(r,s,M.lookup s $ bindings i) | r <- regs, s <- maybeToList $ lookupRegSym r (registers p)]
      parent (_,_,b) = fmap fst b
      groups = classesBy ((==)`on`parent) vars
      storeGroup g = do
        reg <- loadRoot $ parent $ head g
        lift $ mapM_ (store reg) g
        where store base (r,s,b) = do
                n <- maybe (frameAddr s) (return . snd) b
                sz <- varSize s
                tellCode $ st (base,fi n,fi sz) r
      restrict m = do
        free <- get
        let (free',occ) = SB.partition isFree free
            isFree r = isNothing $ lookupRegSym r (registers p)
        put free' >> m >> modify (SB.union occ)
  restrict $ mapM_ storeGroup groups
  lift $ modifyF registersF $ \rs -> foldr BM.delete rs [s | (_,s,_) <- vars]

loadArgs args = do
  lift $ future $ modify $ \f -> f { fregisters = foldr (uncurry BM.insert) (fregisters f)
                                                  [(s,r) | (SymVal Value s,Just r) <- args] }
  noFuture $ do
    (argVal,globVal,isLocal,binding) <- miscInfos
    p <- lift get
    let fixed = mapMaybe snd args
        argAlloc (arg,Nothing) = runKleisli (left $ k f) (argVal arg)
          where f s = state st
                  where st free = fromMaybe (Left s,free)
                                  $ do r <- BM.lookup s (registers p)
                                       guard (SB.member r free)
                                       return (Right r,SB.delete r free)
        argAlloc (_,Just r) = return (Left $ Right r)
        argNew = runKleisli (left $ k allocReg ||| k return)
    modify $ \s -> foldr SB.delete s fixed
    allocs <- mapM argAlloc args >>= mapM argNew 
    let assocs = lefts [left (,arg,bind arg) all | all <- allocs | (arg,_) <- args]
        bind (SymVal _ s) | isLocal s = binding s
                          | otherwise = Nothing
        groups = classesBy ((==)`on`parent) assocs
        parent (_,_,b) = fmap fst b
        loadGroup g = do
          base <- loadRoot $ parent $ head g
          lift $ mapM_ (load base) g
          where load base (r,arg,b) = case argVal arg of
                  Right (n,v) -> tell $ fromBytesN (if n<=4 then 7 else 10) (movi r $< v)
                  Left s -> do
                    n <- maybe (frameAddr s) return $ fmap snd b
                    sz <- varSize s
                    let SymVal t _ = arg
                    tellCode $ if t==Value then ld r (base,fi n,fi sz) else lea r base (fi n)
                
    storeRegs $ lefts allocs
    mapM_ loadGroup groups
    lift $ modifyF registersF $ \m -> foldr (uncurry BM.insert) m [(s,r) | ((SymVal _ s,_),Left r) <- zip args allocs]
    return allocs

defaults args ret = (MemState pregs frame,Future fr)
  where (regArgs,stArgs) = partition ((<=defSize) . bSize) args
        (regs,nonRegs) = zipRest argRegs regArgs
        (retReg:funReg:argRegs) = allocRegs
        pregs = BM.fromList [(bindSym v,r) | (r,v) <- regs]
        frame = foldr (frameAlloc defSize) emptyFrame (stArgs++nonRegs)
        fr | bSize ret<=defSize = BM.singleton (bindSym ret) retReg
           | otherwise = BM.empty

compile (Op BCall d (fun:args)) = withFreeSet $ do
  sizes <- mapM argSize args
  let (MemState regs subFrame,_) = defaults [BindVar id (sz,0) 0 [] | (id,arg) <- argAssocs | sz <- sizes] undefined
      argAssocs = zip (map ID [0..]) args
  (argVal,_,_,binding) <- miscInfos
  storeRegs [rax]
  modify (SB.delete rax)
  func:_ <- loadArgs $ (fun,Nothing):[(arg,r) | (id,arg) <- argAssocs, let r = BM.lookup id regs, isJust r]
  noFuture $ get >>= storeRegs . SB.toList

  top <- lift $ gets (frameTop . frame)
  let storeBig (id,arg) = case lookupAddr id subFrame of
        Just addr -> case argVal arg of
          Left s -> do
            let loadAddr (r,n) = do a <- frameAddr r ; tellCode $ ld r15 (rsp,fi a,defSize) ; return (r15,a)
            (base,n) <- maybe ((rsp,) $< frameAddr s) loadAddr $ binding s
            sz <- argSize arg
            let addrs = [0,defSize..sz]
            sequence_ [tellCode (ld rax (base,fi$n+a,fi sz)++st (rsp,fi$top+defSize+addr+a,fi sz) rax)
                      | a <- addrs, let sz = min defSize (sz-a)]
          Right (s,v) -> tell (fromBytesN (if s<=4 then 7 else 10) (movi r15 $< v))
                         >> tellCode (st (rsp,fi$top+defSize+addr,defSize) rax)
        Nothing -> return ()
  noFuture $ lift $ mapM_ storeBig argAssocs
  tellCode (addi rsp rsp $ fi top)
  let callc (Right (_,v)) = fromBytesN 5 (calli $< v)
      callc (Left r) = fromBytes $ call r
  tell $ callc func
  tellCode (subi rsp rsp $ fi top)
  
compile (Op b d [a,a']) | b`elem`[BAdd,BSub,BMul,BAnd,BOr,BXor] = withFreeSet $ do
  [v,v'] <- loadArgs [(a,Nothing),(a',Nothing)]
  dest <- lift $ getDestReg d
  storeRegs [dest]
  let ops = fromJust $ lookup b [(BAdd,adds),(BSub,subs),(BMul,muls),(BAnd,bwands),(BOr,bwors),(BXor,bwxors)]
      code = case (v,v') of
        (Left r,Left r') -> let (o,_,_,_,_) = ops in fromBytes $ o dest r r'
        (Left r,Right (s,n)) -> let (_,o,_,_,sz) = ops in fromBytesN (sz dest r s) (o dest r $< n) 
        (Right (s,n),Left r) -> let (_,_,o,_,sz) = ops in fromBytesN (sz dest r s) (n >§ \n -> o dest n r)
        (Right (s,n),Right (s',n')) -> let (_,_,_,o,_) = ops in fromBytesN (max s s') (movi dest $< liftM2 o n n')
  tell code
                        | otherwise = return ()
compile (Op b d args@(a:a':t)) | isBinOp b = mapM_ compile $ Op b d [a,a']:[Op b d [SymVal Value d,a''] | a'' <- t]
                               | otherwise = undefined
compile (Op _ _ _) = return ()
compile (Branch v alts) = return ()
compile (Bind bv Nothing) = modifyF frameF (frameAlloc defSize bv) >> return ()
compile (Bind bv _) = return ()
compile Noop = return ()
