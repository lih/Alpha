{-# LANGUAGE ViewPatterns, TupleSections, ParallelListComp, ImplicitParams, NoMonomorphismRestriction #-}
module Specialize.X86_64(arch_x86_64,execStub,initStub,callStub0,callStub1) where

import Control.Monad.Writer.Class

import Control.Arrow
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Writer
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

arch_x86_64 = Arch "x86_64" defSize defaults compile
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
          movi r14 (withSize (f :: Integer))
          call r14
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

defSize = 8
([rax,rcx,rdx,rbx,rsp,rbp,rsi,rdi,r8,r9,r10,r11,r12,r13,r14,r15],allocRegs) =
  (regs,filter (not . (`elem`[rsp,r14,r15])) regs)
  where regs = [0..15] :: [Int]
isAllocReg r = r>=rax && r<r14 && r/=rsp 
oppFlags f = fromJust $ lookup f $ fls++map swap fls
  where fls = [(0xf,0xc),(0x4,0x5),(0xd,0xe)]

fis = fmap fromIntegral ; fi = fromIntegral
k = Kleisli
a <|||> b = runKleisli (k a ||| k b)
leftK f = runKleisli (left $ k f)

bSize (bindSize -> (n,nr)) = n+nr*defSize
numSize n = length $ takeWhile (>0) $ iterate (`shiftR`8) n
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
  Nothing -> movi r15 n >> op def d a r15
codeFun codes (size,n) = listToMaybe [(code,r,s,imm s) | (s,(code,r)) <- codes, s>=size]
  where imm s = liftM (take s . bytes) n
                      
mov d s | d==s = return ()
        | otherwise = tellCode (pre++[0x8b]++suf)
  where (pre,suf) = argBytes d s Nothing
movi d (0,_) = bwxorrr d d d
movi d n = tell $ fromBytesN (length pref+s) (liftM (pref++) imm)
  where (code,r,s,imm) = fromJust $ codeFun [(4,(0xC7,0)),(8,(0xB8`xor`(fi d.&.7),0))] n
        (pre,suf) | code==0xC7 = argBytes 0 d Nothing
                  | otherwise = (fst $ argBytes d 0 Nothing,[])
        pref = pre++[code]++suf
lea d s n = tellCode $ pre++[0x8d]++post
  where (pre,post) = argBytes d s (Just n)
zxtnd r | r>=4 && r<8 = mov r15 r >> zxtnd r15 >> mov r r15
        | otherwise = tellCode $ pre++[0x0f,0xb6]++post
  where (pre,post) = argBytes r r Nothing
setcc r f = tellCode (pre++[0x0f,0x90.|.fi f]++post) >> zxtnd r
  where (pre,post) = argBytesWide False 0 r Nothing

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
            else tellCode $ pre'++pre++code++suf
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
        stChunk (i,sz) lst = do
          (if sz==1 && s>=4 && s<8
            then mov r15 s >> st (d,n+i,1) r15
            else tellCode $ pre'++pre++code++suf)
          sh sz
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
(addrr,addri',addir)      = commOp [0x03] [(1,(0x83,0)),(4,(0x81,0))]
(mulrr,mulri,mulir)       = commOp [0x0F,0xAF] [(1,(0x6B,0)),(8,(0x69,0))]
(bwandrr,bwandri,bwandir) = commOp [0x23]      [(1,(0x83,4)),(4,(0x81,4))]
(bworrr,bworri,bworir)    = commOp [0x0b]      [(1,(0x83,1)),(4,(0x81,1))]
(bwxorrr,bwxorri,bwxorir) = commOp [0x33]      [(1,(0x83,6)),(4,(0x81,6))]
neg r = tellCode $ pre++[0xf7]++post
  where (pre,post) = argBytes 3 r Nothing
subrr d a b | d==b = op [0x2b] d d a >> neg d
            | otherwise = op [0x2b] d a b
subri d r (0,_) = return ()
subri d r v = opi (codeFun [(1,(0x83,5)),(4,(0x81,5))]) [0x2b] d r v
subir d n a | d==a = subri d d n >> neg d
            | otherwise = movi d n >> subrr d d a

cmprr _ a b = op [0x3b] a a b
cmpri _ a = opi (codeFun codes) [0x3b] a a
  where codes = [(1,(0x83,7)),(4,(0x81,7))]
cmpir _ n a = movi r15 n >> cmprr r15 r15 a

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
  (Right (s,n),Right (s',n')) -> movi dest (max s s',liftM2 ii n n')

argVal (IntVal n) = Right $ withSize n
argVal (SymVal Size s) = Right $ withSize $ fromMaybe defSize $ M.lookup s (sizes ?info)
argVal (SymVal SymID (ID s)) = Right $ withSize $ s
argVal (SymVal _ s) = maybe (Left s) (Right . (4,)) $ globVal s
globVal s = if isLocal s then Nothing else Just (toInteger $< snd (envInfo ?info) s)
isLocal s = S.member s (locals ?info)
binding s = M.lookup s (bindings ?info)
isActive s = S.member s (actives ?info)
varSize s = fromMaybe defSize (M.lookup s $ sizes ?info) 
argSize (SymVal Value s) = varSize s
argSize _ = defSize
verbAddress = let (me,addrs) = envInfo ?info in addrs me
instrAddress i = let (_,addrs) = branchPos ?info ; (e,s,_) = addrs i in (e,s) 
instrPast i = let (_,addrs) = branchPos ?info ; (_,_,p) = addrs i in p 
thisInstr = fst $ branchPos ?info

associate r s = modifyF registersF (maybe BM.deleteR BM.insert s r)
frameAddr s = stateF frameF (withAddr defSize s)
stackAddr sz = liftM (frameToStack sz) . frameAddr
frameToStack sz n = -(n+sz)
lookupSymIn = flip BM.lookupR
lookupRegIn = flip BM.lookup
argValSym (SymVal Value s) = Just s
argValSym _ = Nothing
lookupArgReg arg m = argValSym arg >>= lookupRegIn m

withFreeSet m = liftM2 (,) get (future get) >>= \(p,f) -> do
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
  evalStateT m (SB.fromList cmp allocRegs)

readFuture m = StateT s
  where s t = RWTL (\r p f -> let ~(p',_,a,w) = runRWTL (runStateT m t) (r,f) p undef in (p',f,a,w))
        undef = error "Illegal use of protected future"
unReadFuture m = StateT s
  where s t = RWTL (\(r,f) p _ -> runRWTL (runStateT m t) r p f)
preserve m = StateT s
  where s t = RWTL (\r p f -> let (_,_,a,w) = runRWTL (runStateT m t) r p f in (p,f,a,w) )
regInfo = liftM2 (,) (gets registers) (asks (fregisters . snd))

allocReg sym = lift regInfo >>= \(_,regs) -> do
  let st free = (r,SB.delete r free)
        where r | SB.null free = r14
                | otherwise = fromMaybe (SB.findMin free) $ mfilter (`SB.member`free)
                                                                     $ lookupRegIn regs sym
  state st

destRegister d = lift regInfo >>= \(regs,fregs) -> 
  case mfilter (\r -> maybe True (==d) $ BM.lookupR r regs) $ lookupRegIn fregs d of
    Just r -> return r
    Nothing -> case mfilter isAllocReg (findSym d) `mplus` find (isNothing . findReg) allocRegs of
      Just r -> return r
      Nothing -> storeRegs [head allocRegs] >> return (head allocRegs)
      where findReg r = lookupSymIn regs r `mplus` lookupSymIn fregs r
            findSym s = lookupRegIn regs s `mplus` lookupRegIn fregs s

loadRoot (Just s) = lift regInfo >>= \(regs,_) -> case lookupRegIn regs s of
  Just r -> return r
  Nothing -> do
    r <- allocReg s
    a <- lift (stackAddr defSize s)
    lift $ associate r (Just s)
    ld r (rsp,fi a,defSize)
    return r
loadRoot Nothing = return rsp

storeRegs rs = lift regInfo >>= \(regs,_) -> do
  let vars = [(r,s,binding s) | (r,Just s) <- zip rs (map (lookupSymIn regs) rs)]
      parent (_,_,b) = fmap fst b
      groups = classesBy ((==)`on`parent) vars
      storeGroup g = do
        ch <- lift $ gets (flip S.member . changed)
        let loaded = [(ch s,ge) | ge@(_,s,_) <- g]
        reg <- if any fst loaded then loadRoot $ parent $ head g else return undefined
        lift $ mapM_ (\(c,ge@(r,s,_)) -> when c (store reg ge)
                                         >> associate r Nothing
                                         >> modifyF changedF (S.delete s)) loaded
        where store base (r,s,b) = do
                n <- maybe (stackAddr (varSize s) s) (return . snd) b
                st (base,fi n,fi (varSize s)) r
      restrict m = gets (SB.partition isFree) >>= \(free',occ) -> put free' >> m >> modify (SB.union occ)
        where isFree = isNothing . lookupSymIn regs
  restrict $ mapM_ storeGroup groups

loadArgs args = do
  let modFRegs regs = foldr ($) regs [maybe (BM.deleteR r) (flip BM.insert r) $ argValSym arg
                                     | (arg,Just r) <- args]
  lift $ future $ modifyF fregistersF $ modFRegs
  readFuture $ do
    (regs,_) <- lift regInfo
    let fixed = mapMaybe snd args
        argAlloc (arg,Nothing) = leftK f (argVal arg)
          where f s = get >§ \free -> maybe (Left s) Right
                                      $ mfilter (`SB.member` free) (BM.lookup s regs)
        argAlloc (_,Just r) = return (Left $ Right r)
        argNew = leftK (allocReg <|||> return)
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
        myWorkIsDone r s = lookupRegIn regs s == Just r
        loadGroup g = do
          base <- loadRoot (parent $ head g)
          mapM_ (load base) g
          where load base (r,arg,b) = lift regInfo >>= \(regs,_) -> do
                  when (isJust $ lookupSymIn regs r) (storeRegs [r])
                  lift $ case argVal arg of
                    Right v -> movi r v
                    Left s -> case lookupRegIn regs s of
                      Just r' -> mov r r'
                      Nothing -> do
                        n <- maybe (stackAddr (varSize s) s) return $ fmap snd b
                        if symValType arg == Value
                          then ld r (base,fi n,fi (varSize s))
                          else lea r base (fi n)
                  lift $ associate r ((Just ||| const Nothing) $ argVal arg)  

    mapM_ loadGroup groups
    return allocs

alignWith regs = do
  loadArgs [(SymVal Value s,Just r) | (s,r) <- BM.toList regs]
  free <- get
  readFuture $ storeRegs (SB.toList free)

defaults args ret = (MemState pregs (S.fromList $ map (bindSym . snd) regs) frame,Future fr)
  where (regArgs,stArgs) = partition ((<=defSize) . bSize) args
        (regs,nonRegs) = zipRest argRegs regArgs
        (retReg:funReg:argRegs) = allocRegs
        pregs = BM.fromList [(bindSym v,r) | (r,v) <- regs]
        frame = foldr (frameAlloc defSize) emptyFrame (stArgs++nonRegs)
        fr | bSize ret<=defSize = BM.singleton (bindSym ret) retReg
           | otherwise = BM.empty

storeFlags s = do
  regs <- gets registers
  withFreeSet $ readFuture $ case M.lookupGE 16 (BM.toMapR regs) of
    Nothing -> doNothing
    Just (rf,s') | s==Just s' -> doNothing
                 | isActive s' -> do
      r <- destRegister s'
      storeRegs [r]
      setcc r rf
      lift $ associate r (Just s')
                 | otherwise -> lift $ associate rf Nothing

compile i = ask >>= \info -> let ?info = info in 
  listen (storeFlags (branchSym i)) >>= \(_,BC (e',s',_)) -> 
  let ?info = info { branchPos = (thisInstr,
                                  \i -> let (e,s,p) = snd (branchPos ?info) i in (e+e',s+s',p)) }
  in compile' i

  where branchSym (Branch (SymVal Value s) _) = Just s
        branchSym _ = Nothing
                                                 
compile' (Op b d vs) = do
  future $ modifyF fregistersF $ BM.delete d
  compileOp b d vs
  flip evalStateT (SB.empty compare) $ readFuture $ do
    (regs,_) <- lift regInfo
    when (BM.member d regs) $ lift $ modifyF changedF (S.insert d)
    storeRegs [r | (s,r) <- BM.toList regs, not (isActive s), isJust (binding s)]
    lift $ modifyF registersF (BM.filter (const . isActive))
compile' (Branch v alts) = withFreeSet $ do
  let alignPast i = maybe doNothing (preserve . alignWith . registers) (instrPast i)
      jmpc short long (BC (e,s,_)) (BC (e',s',_)) = BC (length long+4,length code,return $ B.pack code)
        where de = e'-e ; ds = s'-s
              code | de==0 = []
                   | de > -128 && de<=128 = short++take 1 (bytes ds)
                   | otherwise = long++take 4 (bytes ds)
      jmp = jmpc [0xeb] [0xe9]
      start i = BC (est,pos,undefined) where (est,pos) = instrAddress i

  case alts of
    [def,null] -> do
      (r,c) <- censor (const mempty) $ listen $ readFuture $ do
        r <- lift $ gets (lookupArgReg v . registers)
        case mfilter (>=16) r of
          Just r -> return r
          Nothing -> do
            [Left r] <- unReadFuture (loadArgs [(v,Nothing)])
            cmpri r r (withSize (0 :: Int))
            return r
      [al,al'] <- mapM (listening . alignPast) [def,null]
      let [_,_,p1,_,p2,_,p3] = scanl mappend (start thisInstr) codes
          (d1,jmp2) = if isEmptyCode al' then (start null,mempty) else (p2,jmp p3 (start null))
          codes = [c,jmpc cshort clong p1 d1,al,jmp p2 (start def),al',jmp2]
          cshort = [0x70+testCode] ; clong = [0x0f,0x80+testCode]
          testCode = oppFlags $ fi $ if r>=16 then r-16 else 0x4
      mapM_ tell codes
    [def] -> do
      al <- listening $ alignPast def
      let [_,_,p] = scanl mappend (start thisInstr) codes
          codes = [al,jmp p (start def)]
      mapM_ tell codes
    [] -> readFuture $ do
      p <- lift get
      let isPresent s _ = isJust $ msum [void $ binding s
                                        ,void $ lookupAddr s (frame p)
                                        ,void $ lookupRegIn (registers p) s]
      (_,fregs) <- lift $ regInfo
      unReadFuture $ alignWith (BM.filter isPresent fregs)
      tellCode [0xc3]

compile' (Bind bv arg) = do
  future $ modifyF fregistersF $ \rs -> foldr BM.delete rs (bindSyms bv)
  when (isNothing arg) $ modifyF frameF (frameAlloc defSize bv)

compile' Noop = withFuture (align . fregisters)
  where align regs = void $ withFreeSet $ loadArgs [(SymVal Value s,Just r) | (s,r) <- BM.toList regs]

compileOp BCall d (fun:args) = withFreeSet $ do
  let (MemState regs _ subFrame,_) = defaults [BindVar id (sz,0) 0 []
                                              | (id,arg) <- argAssocs
                                              | sz <- map argSize args] undefined
      argAssocs = zip (map ID [0..]) args
      storeBig top (id,arg) = case lookupAddr id subFrame of
        Just addr -> case argVal arg of
          Left s -> do
            let loadAddr (r,n) = do a <- stackAddr defSize r ; ld r14 (rsp,fi a,defSize) ; return (r14,n)
            (base,n) <- maybe ((rsp,) $< stackAddr size s) loadAddr $ binding s
            let addrs = [0,defSize..size]
            sequence_ [ld rax (base,fi $ n+a,fi sz)
                       >> st (rsp,fi $ frameToStack size (top+defSize+addr)+a,fi sz) rax
                      | a <- addrs, let sz = min defSize (size-a)]
            
          Right v -> movi r14 v >> st (rsp,fi $ frameToStack defSize $ top+defSize+addr,defSize) r14
          where size = argSize arg
        Nothing -> return ()

  modify (SB.delete rax)
  let args' = [(arg,r :: Maybe Register) | (id,arg) <- argAssocs, let r = BM.lookup id regs, isJust r]
  (func:_,cload) <- listen $ loadArgs $ (fun,Nothing):args'

  readFuture $ do
    put (SB.empty compare)
    (_,cstore) <- listen $ storeRegs allocRegs
    top <- lift $ gets (frameTop . frame)
    (_,cstore') <- listen $ do
      lift $ mapM_ (storeBig top) argAssocs
      subri rsp rsp $ withSize top
    let pos = verbAddress >§ (+(snd (instrAddress thisInstr)+delta))
        BC ~(_,delta,_) = cload <> cstore <> cstore'
    (call <|||> calli pos) func
    addri rsp rsp $ withSize top
    lift $ associate rax (Just d)

compileOp BSet d [s] = withFreeSet $ do
  [v] <- loadArgs [(s,Nothing)]
  readFuture $ do
    let dest r = maybe (destRegister d) (const $ return r) $ mfilter (not . isActive) $ argValSym s
    r' <- dest $ (id ||| const 0) v 
    (mov r' <|||> movi r') v
    lift $ associate r' (Just d)
compileOp b d [a,a'] | b`elem`[BAdd,BSub,BMul,BAnd,BOr,BXor] = withFreeSet $ do
  let ops = fromJust $ lookup b [(BAdd,(addrr,addri,addir,(+)))
                                ,(BSub,(subrr,subri,subir,(-)))
                                ,(BMul,(mulrr,mulri,mulir,(*)))
                                ,(BAnd,(bwandrr,bwandri,bwandir,(.&.)))
                                ,(BOr,(bworrr,bworri,bworir,(.|.)))
                                ,(BXor,(bwxorrr,bwxorri,bwxorir,xor))]
  [v,v'] <- loadArgs [(a,Nothing),(a',Nothing)]
  readFuture $ do
    dest <- destRegister d
    opsCode ops dest v v'
    lift $ associate dest (Just d)
                        | b`elem`[BLowerThan,BLowerEq,BEqual,BNotEqual,BGreaterEq,BGreaterThan] = withFreeSet $ do
  let dest = 16 + fromJust (lookup b codes)
      codes = [(BLowerThan,0xc),(BLowerEq,0xe),(BGreaterEq,0xf),(BGreaterThan,0xd),(BEqual,0x4),(BNotEqual,0x5)]
      applys = [(BLowerThan,(<)),(BLowerEq,(<=)),(BGreaterEq,(>=)),(BGreaterThan,(>)),(BEqual,(==)),(BNotEqual,(/=))]
      convert f n n' = if f n n' then 1 else 0
  [v,v'] <- loadArgs [(a,Nothing),(a',Nothing)]
  readFuture $ do
    opsCode (cmprr,cmpri,cmpir,convert $ fromJust (lookup b applys)) dest v v'
    lift $ associate dest (Just d)
                        | b`elem`[BMod,BDiv] = withFreeSet $ do
  [_,_,v] <- loadArgs [(a,Just rax),(IntVal 0,Just rdx),(a',Nothing)]
  readFuture $ do
    case mfilter (/=d) $ argValSym a of
      Just s | isActive s -> storeRegs [rax]
      _ -> return ()
    case v of
      Left r -> op [0xf7] 7 7 r
      Right v -> opi (codeFun []) [0xf7] 7 7 v
  lift $ associate (if b==BMod then rdx else rax) (Just d)

compileOp b d args@(a:a':t) | isBinOp b =
  sequence_ [compileOp b d [a,a']
            | b <- repeat b
            | d <- repeat d
            | a <- a:repeat (SymVal Value d)
            | a' <- a':t]
compileOp _ _ _ = return ()
