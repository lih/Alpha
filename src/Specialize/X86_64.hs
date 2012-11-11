{-# LANGUAGE ViewPatterns, TupleSections, ParallelListComp, ImplicitParams, NoMonomorphismRestriction, Rank2Types #-}
module Specialize.X86_64(arch_x86_64) where

import Control.Monad.Writer.Class

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
codeFun      :: [(Int,(Word8,Int))] -> (Int,IO Integer) -> Maybe (Word8,Int,Int,IO [Word8])

type OpT = MonadWriter BinCode m => (Int -> Int -> Int -> m ()
                                    ,Int -> Int -> (Int,IO Integer) -> m ()
                                    ,Int -> (Int,IO Integer) -> Int -> m ()
                                    ,Integer -> Integer -> Integer)

arch_x86_64 = Arch "x86_64" defSize defaults compile

defSize = 8
([rax,rcx,rdx,rbx,rsp,rbp,rsi,rdi,r8,r9,r10,r11,r12,r13,r14,r15],allocRegs) =
  (regs,filter (not . (`elem`[rsp,r15])) regs)
  where regs = [0..15] :: [Int]

fis = fmap fromIntegral ; fi = fromIntegral
k = Kleisli

showHex n = reverse $ map (intToDigit . fromIntegral . (`mod`16)) $ take 2 $ iterate (`div`16) (n :: Word8)
showCode = intercalate " " . map showHex

bSize (bindSize -> (n,nr)) = n+nr*defSize
numSize n = length $ takeWhile (>0) $ iterate (`shiftR`8) n
withSize n = (numSize n,return $ fi n)
fromFields fs = foldl1 xor (zipWith shiftL (map (fst) fs) (scanl (+) 0 $ map snd fs))
bytes = fis . iterate (`shiftR`8)
frameToStack n = (-8)-fi n

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
  Nothing -> movi r15 n >> op def d a r15
codeFun codes (size,n) = listToMaybe [(code,r,s,imm s) | (s,(code,r)) <- codes, s>=size]
  where imm s = liftM (take s . bytes) n
                      
mov d s | d==s = return ()
        | otherwise = tellCode $ pre++[0x8b]++suf
  where (pre,suf) = argBytes d s Nothing
movi d (0,_) = bwxor d d d
movi d n = tell $ fromBytesN (length pref+s) (liftM (pref++) imm)
  where (code,r,s,imm) = fromJust $ codeFun [(4,(0xC7,0)),(8,(0xB8`xor`(fi d.&.7),0))] n
        (pre,suf) | code==0xC7 = argBytes 0 d Nothing
                  | otherwise = (fst $ argBytes d 0 Nothing,[])
        pref = pre++[code]++suf
lea d s n = tellCode $ pre++[0x8d]++post
  where (pre,post) = argBytes d s (Just n)

shli = opi (codeFun [(1,(0xC1,4))]) undefined
shri = opi (codeFun [(1,(0xC1,5))]) undefined
rori d s n | n==0||n==64 = return ()
           | otherwise = opi (codeFun [(1,(0xC1,1))]) undefined d s (withSize n)
ld d (_,_,0) = return ()
ld d (s,n,size) = load
  where szs = maximumBy (comparing weight) $ permutations [sz | sz <- [8,4,2,1], sz.&.size /= 0]
          where weight l = sum $ zipWith f l $ sums l
                  where f s i = fromJust $ findIndex (\p -> m.&.p==0) $ iterate (`shiftR`1) s
                          where m = s-((n+i)`mod`s)
        load = sequence_ $ zipWith ldChunk (reverse $ zip (sums szs) szs) (True:repeat False)
        ldChunk (i,sz) fst = do
          sh sz
          if sz==1 && d>=4 && d<8
            then (if fst then return () else mov r15 d) >> ld r15 (s,n+i,1) >> mov d r15
            else tellCode $ pre++code++suf
          where (pre,suf) = argBytesWide (sz==8) d s (Just (n+i))
                code = fromJust (lookup sz [(8,[0x8b]),(4,[0x8b]),(2,[0x66,0x8b]),(1,[0x8a])])
                sh sz | fst||sz==8 = return ()
                      | otherwise = shli d d (withSize (sz*8))
st (_,_,0) _ = return ()
st (d,n,size) s = store
  where szs = maximumBy (comparing weight) $ permutations [sz | sz <- [8,4,2,1], sz.&.size /= 0]
          where weight l = sum $ zipWith f l $ sums l
                  where f s i = fromJust $ findIndex (\p -> m.&.p==0) $ iterate (`shiftR`1) s
                          where m = s-((n+i)`mod`s)
        store = sequence_ $ reverse [stChunk a b | (a,b) <- zip (reverse $ zip (sums szs) szs) (True:repeat False)]
        stChunk (i,sz) lst = do
          (if sz==1 && d>=4 && d<8
            then mov r15 s >> st (d,n+i,1) r15
            else tellCode $ pre++code++suf)
          sh sz
          where (pre,suf) = argBytesWide (sz==8) s d (Just (n+i))
                code = fromJust (lookup sz [(8,[0x89]),(4,[0x89]),(2,[0x66,0x89]),(1,[0x88])])
                sh sz | lst = rori s s ((8-i)*8)
                      | otherwise = rori s s (sz*8)

commOp c c' f = (op c,opn,flip . opn,f :: Integer -> Integer -> Integer)
  where opn = opi (codeFun c') c

adds@(add,addi,_,_)       = commOp [0x03]      [(1,(0x83,0)),(4,(0x81,0))]  (+)
muls@(mul,muli,_,_)       = commOp [0x0F,0xAF] [(1,(0x6B,0)),(8,(0x69,0))]  (*)
bwands@(bwand,bwandi,_,_) = commOp [0x23]      [(1,(0x83,4)),(4,(0x81,4))]  (.&.)
bwors@(bwor,bwori,_,_)    = commOp [0x0b]      [(1,(0x83,1)),(4,(0x81,1))]  (.|.)
bwxors@(bwxor,bwxori,_,_) = commOp [0x33]      [(1,(0x83,6)),(4,(0x81,6))]  xor
subs :: OpT
subs@(sub,subi,_,_) = (sub,opi (codeFun codes) [0x2b],subi',(-))
  where neg r = tellCode $ pre++[0xf7]++post
          where (pre,post) = argBytes 3 r Nothing
        sub d a b | d==b = op [0x2b] d d a >> neg d
                  | otherwise = op [0x2b] d a b
        codes = [(1,(0x83,5)),(4,(0x81,5))]
        subi' d n a | d==a = subi d d n >> neg d
                    | otherwise = movi d n >> sub d d a
cmps f = (cmp,cmpi,cmp',f)
  where cmp _ a b = op [0x3b] a a b
        codes = [(1,(0x83,7)),(4,(0x81,7))]
        cmpi _ a = opi (codeFun codes) [0x3b] a a
        cmp' _ n a = movi r15 n >> cmp r15 r15 a

calli pos (_,v) = tell $ fromBytesN 5 (liftM2 (\p v -> [0xe8]++take 4 (bytes (v-fi p-5))) pos v)
call r = tellCode $ pre++[0xff]++post
  where (pre,post) = argBytes 2 r Nothing

opsCode (rr,ri,ir,ii) dest v v' = case (v,v') of
  (Left r,Left r') -> rr dest r r'
  (Left r,Right v) ->  ri dest r v
  (Right v,Left r) -> ir dest v r
  (Right (s,n),Right (s',n')) -> movi dest (max s s',liftM2 ii n n')

associate d r = modifyF registersF $ BM.insert d r
frameAddr s = stateF frameF (withAddr defSize s)
lookupSymIn = flip BM.lookupR
lookupRegIn = flip BM.lookup
lookupArgReg (SymVal Value s) m = BM.lookup s m
lookupArgReg _ _ = Nothing

miscInfos = ask >>= \info -> do
  let argVal (IntVal n) = Right $ withSize n
      argVal (SymVal Size s) = Right $ withSize $ fromMaybe defSize $ M.lookup s (sizes info)
      argVal (SymVal SymID (ID s)) = Right $ withSize $ s
      argVal (SymVal _ s) = maybe (Left s) (Right . (4,)) $ globVal s
      globVal s = if isLocal s then Nothing else Just (fi $< snd (envInfo info) s)
      isLocal s = S.member s (locals info)
      binding s = M.lookup s (bindings info)
  return (argVal,globVal,isLocal,binding)

withFreeSet m = liftM3 (,,) ask get (future get) >>= \(i,p,f) -> do
  let cmp r r' = case (regVar r,regVar r') of
        (Just _,Nothing) -> GT
        (Nothing,Just _) -> LT
        (Nothing,Nothing) -> case (fRegVar r,fRegVar r') of
          (Just _,Nothing) -> GT
          (Nothing,Just _) -> LT
          _ -> compare r r'
        (Just v,Just v') -> (isActive v`compare`isActive v')`mappend`compare r r'
      regVar = lookupSymIn $ registers p
      fRegVar = lookupSymIn $ fregisters f
      isActive v = S.member v $ actives i
  evalStateT m (SB.fromList cmp allocRegs)

protectFuture m = StateT s
  where s t = RWTL (\r p f -> let ~(p',_,a,w) = runRWTL (runStateT m t) (r,f) p undef in (p',f,a,w))
        undef = error "Illegal use of protected future"
unProtectFuture m = StateT s
  where s t = RWTL (\(r,f) p _ -> runRWTL (runStateT m t) r p f)
preserve m = StateT s
  where s t = RWTL (\r p f -> let (_,f',a,w) = runRWTL (runStateT m t) r p f in (p,f',a,w) )

infos = liftM3 (,,) (asks fst) get (asks snd)
varSize s = fromMaybe defSize $< asks (M.lookup s . sizes . fst) 
argSize (SymVal Value s) = varSize s
argSize _ = return defSize

allocReg sym = asks (fregisters . snd) >>= \regs -> do
  let st free = (r,SB.delete r free)
        where r | SB.null free = r15
                | otherwise = fromMaybe (SB.findMin free) $ mfilter (`SB.member`free) $ lookupRegIn regs sym
  r <- state st
  return r

destRegister d = lift infos >>= \(i,p,f) -> 
  case mfilter (\r -> maybe True (==d) $ BM.lookupR r (registers p)) $ lookupRegIn (fregisters f) d of
    Just r -> return r
    Nothing -> case find ((==Just d) . findReg) allocRegs `mplus` find (isNothing . findReg) allocRegs of
      Just r -> return r
      Nothing -> storeRegs [head allocRegs] >> return (head allocRegs)
      where findReg s = lookupSymIn (registers p) s `mplus` lookupSymIn (fregisters f) s

loadRoot (Just s) = lift get >>= \p -> case lookupRegIn (registers p) s of
  Just r -> return r
  Nothing -> do
    r <- allocReg s
    a <- lift (frameAddr s)
    lift $ associate s r
    ld r (rsp,fi a,defSize)
    return r
loadRoot Nothing = return rsp

storeRegs regs = lift get >>= \p -> do
  (_,_,_,binding) <- unProtectFuture miscInfos
  let vars = [(r,s,binding s) | (r,Just s) <- zip regs (map (lookupSymIn (registers p)) regs)]
      parent (_,_,b) = fmap fst b
      groups = classesBy ((==)`on`parent) vars
      storeGroup g = do
        reg <- loadRoot $ parent $ head g
        mapM_ (store reg) g
        where store base (r,s,b) = do
                n <- maybe (lift $ frameAddr s) (return . snd) b
                sz <- varSize s
                st (base,frameToStack n,fi sz) r
      restrict m = gets (SB.partition isFree) >>= \(free',occ) -> put free' >> m >> modify (SB.union occ)
        where isFree = isNothing . lookupSymIn (registers p)
  restrict $ mapM_ storeGroup groups
  lift $ modifyF registersF $ \rs -> foldr BM.delete rs [s | (_,s,_) <- vars]

loadArgs args = do
  lift $ future $ modify $ \f -> f { fregisters = foldr (uncurry BM.insert) (fregisters f)
                                                  [(s,r) | (SymVal Value s,Just r) <- args] }
  protectFuture $ do
    (argVal,globVal,isLocal,binding) <- unProtectFuture miscInfos
    p <- lift get
    let fixed = mapMaybe snd args
        argAlloc (arg,Nothing) = runKleisli (left $ k f) (argVal arg)
          where f s = get >§ \free -> maybe (Left s) Right $ mfilter (`SB.member` free) (BM.lookup s (registers p))
        argAlloc (_,Just r) = return (Left $ Right r)
        argNew = runKleisli (left $ k allocReg ||| k return)
    modify $ \s -> foldr SB.delete s fixed
    alls <- mapM argAlloc args
    modify $ \s -> foldr SB.delete s [r | Left (Right r) <- alls]
    allocs <- mapM argNew alls
    let assocs = filter (\(r,arg,_) -> not $ (myWorkIsDone r ||| const False) (argVal arg))
                 $ lefts [left (,arg,bind arg) all | all <- allocs | (arg,_) <- args]
        bind (SymVal _ s) | isLocal s = binding s
        bind _ = Nothing
        groups = classesBy ((==)`on`parent) assocs
        parent (_,_,b) = fmap fst b
        myWorkIsDone r s = lookupRegIn (registers p) s == Just r
        loadGroup g = do
          base <- loadRoot $ parent $ head g
          mapM_ (load base) g
          where load base (r,arg,b) = case argVal arg of
                  Right v -> movi r v
                  Left s -> do
                    n <- maybe (lift $ frameAddr s) return $ fmap snd b
                    sz <- varSize s
                    let SymVal t _ = arg
                    if t==Value then ld r (base,frameToStack n,fi sz) else lea r base (fi n)
                
    storeRegs [r | (r,_,_) <- assocs]
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
  (instr,brInfo) <- asks branchPos
  (myID,addrs) <- asks envInfo
  sizes <- protectFuture $ mapM argSize args
  let (MemState regs subFrame,_) = defaults [BindVar id (sz,0) 0 [] | (id,arg) <- argAssocs | sz <- sizes] undefined
      argAssocs = zip (map ID [0..]) args
      start = BC (est,pos,undefined) where (est,pos,_) = brInfo instr
  (argVal,_,_,binding) <- miscInfos
  modify (SB.delete rax)
  (func:_,cload) <- listen $ loadArgs $ (fun,Nothing):[(arg,r) | (id,arg) <- argAssocs, let r = BM.lookup id regs, isJust r]
  protectFuture $ do
    (_,cstore) <- listen $ get >>= \free -> storeRegs (rax:SB.toList free)

    top <- lift $ gets (frameTop . frame)
    let storeBig (id,arg) = case lookupAddr id subFrame of
          Just addr -> case argVal arg of
            Left s -> do
              let loadAddr (r,n) = do a <- frameAddr r ; ld r15 (rsp,fi a,defSize) ; return (r15,a)
              (base,n) <- maybe ((rsp,) $< frameAddr s) loadAddr $ binding s
              sz <- argSize arg
              let addrs = [0,defSize..sz]
              sequence_ [ld rax (base,fi$n+a,fi sz) >> st (rsp,fi $ top+defSize+addr+a,fi sz) rax
                        | a <- addrs, let sz = min defSize (sz-a)]
            Right v -> movi r15 v >> st (rsp,fi $ top+defSize+addr,defSize) rax
          Nothing -> return ()
    (_,cstore') <- listen $ do
      lift $ mapM_ storeBig argAssocs
      subi rsp rsp $ withSize top
    let pos = thisFunc >§ \p -> p+instrPos+delta
        BC ~(_,delta,_) = cload <> cstore <> cstore'
        (_,instrPos,_) = brInfo instr ; thisFunc = addrs myID
    runKleisli (k call ||| k (calli pos)) func
    addi rsp rsp $ withSize top

compile (Op BSet d [s]) = withFreeSet $ do
  [v] <- loadArgs [(s,Nothing)]
  protectFuture $ do
    dest <- destRegister d
    runKleisli (k (mov dest) ||| k (movi dest)) v
    lift $ associate d dest
compile (Op b d [a,a']) | b`elem`[BAdd,BSub,BMul,BAnd,BOr,BXor] = withFreeSet $ do
  let ops = fromJust $ lookup b [(BAdd,adds),(BSub,subs),(BMul,muls),(BAnd,bwands),(BOr,bwors),(BXor,bwxors)]
  [v,v'] <- loadArgs [(a,Nothing),(a',Nothing)]
  protectFuture $ do
    dest <- destRegister d
    opsCode ops dest v v'
    lift $ associate d dest
                        | b`elem`[BLowerThan,BLowerEq,BEqual,BNotEqual,BGreaterEq,BGreaterThan] = withFreeSet $ do
  let dest = 16 + fromJust (lookup b codes)
      codes = [(BLowerThan,0xf),(BLowerEq,0xd),(BGreaterEq,0xc),(BGreaterThan,0xe),(BEqual,0x5),(BNotEqual,0x4)]
      applys = [(BLowerThan,(<)),(BLowerEq,(<=)),(BGreaterEq,(>=)),(BGreaterThan,(>)),(BEqual,(==)),(BNotEqual,(/=))]
      convert f n n' = if f n n' then 1 else 0
  [v,v'] <- loadArgs [(a,Nothing),(a',Nothing)]
  protectFuture $ do
    opsCode (cmps $ convert $ fromJust (lookup b applys)) dest v v'
    lift $ associate d dest
                        | b`elem`[BMod,BDiv] = withFreeSet $ do
  protectFuture $ storeRegs [rdx]
  movi rdx $ withSize (0 :: Int)
  [_,v] <- loadArgs [(a,Just rax),(a',Nothing)]
  protectFuture $ case v of
    Left r -> op [0xf7] 7 7 r
    Right v -> opi (codeFun []) [0xf7] 7 7 v
  lift $ associate d (case b of BMod -> rdx ; BDiv -> rax)

compile (Op b d args@(a:a':t)) | isBinOp b = mapM_ compile $ Op b d [a,a']:[Op b d [SymVal Value d,a''] | a'' <- t]
                               | otherwise = undefined
compile i@(Op _ _ _) = return ()
compile (Branch v alts) = withFreeSet $ do
  (instr,brInfo) <- asks branchPos
  let alignPast instr = preserve $ case brInfo instr of
        (_,_,Just (registers -> regs)) -> protectFuture $ listening $ do
          unProtectFuture $ loadArgs [(SymVal Value s,Just r) | (s,r) <- BM.toList regs]
          free <- get
          storeRegs (SB.toList free)
        _ -> return mempty
      jmpc short long (BC (e,a,_)) (BC (e',a',_)) = BC (length long+4,length code,return $ B.pack code)
        where de = e'-e ; da = a'-a
              code | de==0 = []
                   | de > -128 && de<=128 = short++take 1 (bytes da)
                   | otherwise = long++take 4 (bytes da)
      jmp = jmpc [0xeb] [0xe9]
      start i = BC (est,pos,undefined) where (est,pos,_) = brInfo i

  case alts of
    [def,null] -> do
      (r,c) <- listen $ protectFuture $ do
        r <- lift $ gets (lookupArgReg v . registers)
        case mfilter (>=16) r of
          Just r -> return r
          Nothing -> unProtectFuture (loadArgs [(v,Nothing)]) >§ \[Left r] -> r
      [al,al'] <- mapM alignPast [def,null]
      let [_,_,p1,_,p2,_,p3] = scanl mappend (start instr) codes
          (d1,jmp2) = if isEmptyCode al' then (start null,mempty) else (p2,jmp p3 (start null))
          codes = [c,jmpc cshort clong p1 d1,al,jmp p2 (start def),al',jmp2]
          cshort = [0x70+testCode] ; clong = [0x0f,0x80+testCode] ; testCode = fi $ r-16
      mapM_ tell codes
    [def] -> do
      al <- alignPast def
      let [_,_,p] = scanl mappend (start instr) codes
          codes = [al,jmp p (start def)]
      mapM_ tell codes
    [] -> tellCode [0xc3]

compile (Bind bv Nothing) = modifyF frameF (frameAlloc defSize bv) >> return ()
compile (Bind bv _) = return ()
compile Noop = return ()

ignore m = return ()
        