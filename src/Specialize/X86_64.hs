{-# LANGUAGE ViewPatterns, TupleSections, ParallelListComp, ImplicitParams, NoMonomorphismRestriction #-}
module Specialize.X86_64(arch,execStub,initStub,callStub0,callStub1) where

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
import ID
import My.Control.Monad
import My.Control.Monad.RWTL
import My.Control.Monad.State
import My.Data.Either
import My.Data.List
import My.Prelude
import PCode
import Specialize.Frame
import Specialize.Types
import Specialize.X86_64.Binary
import qualified Data.Bimap as BM
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Set as S
import qualified My.Data.Relation as R
import qualified My.Data.SetBy as SB

arch = Arch "x86-64" defSize defaults compile

allocRegs = filter isAllocReg registers
isAllocReg r = (r>=rax && r<rsp) || (r>rdi && r<=r15)
oppFlags f = fromJust $ lookup f $ fls++map swap fls
  where fls = [(0xf,0xc),(0x4,0x5),(0xd,0xe)]

k = Kleisli
a <|||> b = runKleisli (k a ||| k b)
leftK f = runKleisli (left $ k f)

argVal (IntVal n) = Right $ Left n
argVal (SymVal Size s) = Right $ Left $ fi $ fromMaybe defSize $ M.lookup s (sizes ?info)
argVal (SymVal SymID (ID s)) = Right $ Left $ fi s
argVal (SymVal GValue s) = Right $ Right (defSize*8,toInteger $< snd (envInfo ?info) s)
argVal (SymVal t s) = Left (t,s)
valSym (SymVal Value s) = Just s
valSym _ = Nothing
binding s = M.lookup s (bindings ?info)
isActive s = S.member s (actives ?info)
varSize s = fromMaybe defSize (M.lookup s $ sizes ?info) 
argSize = maybe defSize varSize . valSym
verbAddress = let (me,addrs) = envInfo ?info in addrs me
instrAddress i = let (_,addrs) = branchPos ?info ; (e,s,_) = addrs i in (e,s) 
instrPast i = let (_,addrs) = branchPos ?info ; (_,_,p) = addrs i in p 
thisInstr = fst $ branchPos ?info

frameAddr s = viewState frame_ (withAddr defSize s)
stackAddr sz = liftM (frameToStack sz) . frameAddr
frameToStack sz n = -(n+sz)

associateLoc l s = modifying locations_ $ R.setDom l s
associateVar v s = modifying locations_ $ R.setRan v s
associateVL v l = associateVar v S.empty >> associateLoc l (S.singleton v)
associateReg = associateLoc . Register
associateVR v = associateVL v . Register

withFreeSet m = withFuture $ \f -> get >>= \p -> do
  let cmp r r' = compare (pCount r) (pCount r')
                 `mappend` compare (fCount r) (fCount r')
                 `mappend` compare r r'
      varCount r = S.size . R.lookupDom (Register r)
      pCount = flip varCount (locations p) ; fCount = flip varCount (flocations f)
  evalStateT m (SB.fromList cmp allocRegs)

readFuture m = StateT s
  where s t = RWTL (\r p f -> let ~(p',_,a,w) = runRWTL (runStateT m t) (r,f) p undef in (p',f,a,w))
        undef = error "Illegal use of protected future"
unReadFuture m = StateT s
  where s t = RWTL (\(r,f) p _ -> runRWTL (runStateT m t) r p f)
preserve m = StateT s
  where s t = RWTL (\r p f -> let (_,_,a,w) = runRWTL (runStateT m t) r p f in (p,f,a,w) )
locInfo = liftM2 (,) (gets locations) (asks (flocations . snd))

allocReg sym = lift locInfo >>= \(_,locs) -> do
  let st free = (r,SB.delete r free)
        where r | SB.null free = rdi
                | otherwise = fromMaybe (SB.findMin free) $ mfilter (`SB.member`free) $ symReg sym locs
  state st

destRegister d = lift locInfo >>= \(locs,flocs) -> 
  case mfilter (\r -> S.null $ S.delete d $ R.lookupDom (Register r) locs) $ symReg d flocs of
    Just r -> return r
    Nothing -> case mfilter isAllocReg (regOf d) `mplus` find (null . symsOf) allocRegs of
      Just r -> return r
      Nothing -> saveRegs [head allocRegs] >> return (head allocRegs)
      where symsOf r = regSyms r locs `mplus` regSyms r flocs
            regOf s = symReg s locs `mplus` symReg s flocs

loadRoot (Just s) = lift locInfo >>= \(locs,_) -> case symReg s locs of
  Just r -> return r
  Nothing -> do
    r <- allocReg s ; a <- lift (stackAddr defSize s)
    ld r (rsp,fi a,defSize)
    lift $ associateReg r (S.singleton s)
    return r
loadRoot Nothing = return rsp

saveRegs rs = lift locInfo >>= \(locs,_) -> saveVars $ nubOrd $ concatMap (flip regSyms locs) rs
storeRegs rs = lift locInfo >>= \(locs,_) -> storeVars $ nubOrd $ concatMap (flip regSyms locs) rs
saveVars = saveVars' (\p s -> R.member s Memory (locations p) || S.size (R.lookupRan s (locations p)) > 1)
storeVars = saveVars' (\p s -> R.member s Memory (locations p))
saveVars' isSaved vs = lift locInfo >>= \(locs,_) -> do
  let assocs = [(r,s,binding s) | (s,Just r) <- zip vs (map (flip symReg locs) vs)]
      groups = classesBy ((==)`on`assocRoot) assocs
      assocRoot (_,_,b) = fmap fst b
      storeGroup group = do
        isSaved <- lift $ gets isSaved
        let loaded = [(isSaved s,a) | a@(_,s,_) <- group]
        when (any (not . fst) loaded) $ do
          root <- loadRoot $ assocRoot $ head group
          lift $ mapM_ (\(saved,a) -> unless saved (store root a)) loaded
        where store root (r,s,b) = do
                n <- maybe (stackAddr (varSize s) s) (return . snd) b
                st (root,fi n,fi (varSize s)) r
                modifying locations_ (R.insert s Memory)
      restrict m = gets (SB.partition isFree) >>= \(free',occ) -> put free' >> m >> modify (SB.union occ)
        where isFree r = null $ regSyms r locs
      
  restrict $ mapM_ storeGroup groups

loadArgs args = do
  let modFlocs = [maybe (R.setDom (Register r) S.empty) (\s -> R.insert s (Register r)) $ valSym arg | (arg,Just r) <- args]
  lift $ future $ viewing flocations_ $ mapM_ modify modFlocs
  readFuture $ do
    (locs,_) <- lift locInfo
    let fixed = mapMaybe snd args
        argAlloc (arg,Nothing) = leftK f (argVal arg)
          where f (_,s) = get >§ \free -> maybe (Left s) Right $ mfilter (`SB.member` free) (symReg s locs)
        argAlloc (_,Just r) = return (Left $ Right r)
        argNew = leftK (allocReg <|||> return)
    modify $ SB.deleteMany fixed
    alls <- mapM argAlloc args
    modify $ SB.deleteMany [r | Left (Right r) <- alls]
    allocs <- mapM argNew alls
    let assocs = filter (\(r,arg,_) -> not $ (myWorkIsDone r ||| const False) (argVal arg))
                 $ lefts [left (,arg,bind arg) all | all <- allocs | (arg,_) <- args]
          where bind (SymVal t s) | t`elem`[Value,Address] = binding s
                bind _ = Nothing
        groups = classesBy ((==)`on`parent) assocs
        parent (_,_,b) = fmap fst b
        myWorkIsDone r (_,s) = R.member s (Register r) locs
        loadGroup g = do
          base <- loadRoot (parent $ head g)
          mapM_ (load base) g
          where load base (r,arg,b) = do
                  saveRegs [r] ; lift $ case argVal arg of
                    Right v -> movi r v >> associateReg r S.empty
                    Left (t,s) -> locInfo >>= \(locs,_) -> case (symReg s locs,t) of
                      (Just r',Value) -> mov r r' >> associateReg r (R.lookupDom (Register r') locs)
                      (_,t) -> do
                        n <- maybe (stackAddr (varSize s) s) (return . snd) b
                        case t of
                          Value -> ld r (base,fi n,fi (varSize s)) >> associateReg r (S.singleton s)
                          Address -> lea r base (fi n) >> associateReg r S.empty

    mapM_ loadGroup groups
    return allocs

alignWith locs = do
  loadArgs [(SymVal Value s,Just r) | (s,Register r) <- R.toList locs]
  readFuture $ storeVars $ S.toList $ R.lookupDom Memory locs

defaults args ret = (MemState plocs frame,Future fr)
  where (regArgs,stArgs) = partition (bSize >>> (<=defSize)) args
        (regs,nonRegs) = zipRest argRegs regArgs
        (retReg:funReg:argRegs) = allocRegs
        plocs = R.fromList $ [(bindSym v,Register r) | (r,v) <- regs]
                ++ [(bindSym v,Memory) | v <- stArgs++nonRegs]
        frame = foldr (frameAlloc defSize) emptyFrame (stArgs++nonRegs)
        fr = case ret of
          Just ret | bSize ret<=defSize -> R.singleton (bindSym ret) (Register retReg)
          _ -> R.empty

storeFlags s = withFreeSet $ readFuture $ lift locInfo >>= \(locs,_) -> case R.lookupDom (Flags 0) locs of
  vs | S.null (maybe id S.delete s vs) -> doNothing
     | otherwise -> do
       let s = S.findMin vs
           Flags rf = fromJust (find isFlags $ symLocs s locs)
       r <- destRegister s
       saveRegs [r]
       setcc r rf
       lift $ modifying locations_ $ R.setDom (Register r) vs . R.setDom (Flags 0) S.empty
     
compile i = ask >>= \info -> do
  let ?info = info 
  (_,BC (e',s',_)) <- listen (storeFlags (branchSym i))  
  let ?info = info { branchPos = (thisInstr,
                                  \i -> let (e,s,p) = snd (branchPos ?info) i in (e+e',s+s',p)) }
  compile' i
  modifying locations_ (R.filterDom isActive)
  where branchSym (Branch (SymVal Value s) _) = Just s
        branchSym _ = Nothing
                                                 
compile' (Op b d vs) = do
  future $ modifying flocations_ $ R.setRan d S.empty
  compileOp b d vs
  flip evalStateT (SB.empty compare) $ readFuture $ do
    (locs,_) <- lift locInfo
    saveVars [s | r <- allocRegs, s <- regSyms r locs, not (isActive s), isJust (binding s)]
compile' (Branch v alts) = withFreeSet $ do
  let alignPast i = snd $< listening $ maybe doNothing (\p -> preserve $ do
                                                           lift $ putting frame_ (frame p)
                                                           alignWith $ locations p) (instrPast i)
      jmpc short long (BC ~(e,s,_)) (BC ~(e',s',_)) = BC (length long+4,length code,return $ B.pack code)
        where de = e'-e ; ds = s'-s
              code | de==0 = []
                   | de > -128 && de<=128 = short++take 1 (bytes ds)
                   | otherwise = long++take 4 (bytes ds)
      jmp = jmpc [0xeb] [0xe9]
      start i = BC (est,pos,undefined) where (est,pos) = instrAddress i

  case alts of
    [] -> readFuture $ do
      p <- lift get
      let isPresent s = or [M.member s (bindings ?info)
                           ,isJust $ lookupAddr s (frame p)
                           ,S.member s (R.domain $ locations p)]
      (_,flocs) <- lift locInfo
      unReadFuture $ alignWith (R.filterDom isPresent flocs)
      tellCode [0xc3]
    [def] -> do
      al <- alignPast def
      let [_,_,p] = scanl mappend (start thisInstr) codes
          codes = [al,jmp p (start def)]
      mapM_ tell codes
    [def,null] -> do
      (r,c) <- listening $ readFuture $ do
        rs <- lift $ gets (\p -> maybe [] (flip symLocs (locations p)) (valSym v))
        case find isFlags rs of
          Just f -> return f
          Nothing -> do
            [Left r] <- unReadFuture (loadArgs [(v,Nothing)])
            cmpri r r (Left 0)
            return (Register r)
      [al,al'] <- mapM alignPast [def,null]
      let [_,_,p1,_,p2,_,p3] = scanl mappend (start thisInstr) codes
          (d1,jmp2) = if isEmptyCode al' then (start null,mempty) else (p2,jmp p3 (start null))
          codes = [c,jmpc cshort clong p1 d1,al,jmp p2 (start def),al',jmp2]
          cshort = [0x70+testCode] ; clong = [0x0f,0x80+testCode]
          testCode = oppFlags $ fi $ case r of Flags f -> f ; _ -> 0x4
      mapM_ tell codes
    alts@(def:rest) -> readFuture $ do
      ([Left r],c) <- listen $ unReadFuture $ loadArgs [(v,Nothing)]
      (_,c') <- listen $ cmpri r r (Left $ fi (length alts))
      (alDef:alRest) <- mapM alignPast alts
      let codes = [jmpc [0x72] [0x0f,0x82] cmpP testP
                  ,alDef <> jmp testP (start def)
                  ,execWriter $ do
                    movi rsi (Right (64,posAddr tableP))
                    tellCode [fi $ fromFields [(0,1),(r`shiftR`3,1),(0x12,6)]
                             ,0xff,0x24
                             ,fi $ fromFields [(0x6,3),(r.&.7,3),(0x3,2)]]
                  ,fromBytesN (defSize*(length alts-1)) (concat $< mapM (liftM (take defSize . bytes) . posAddr)
                                                         restDsts)]
                  ++ restCodes
          (restDsts,restCodes) = unzip [if isEmptyCode al
                                        then (start alt,mempty)
                                        else (p, let p' = p<>c ; c = al<>jmp p' (start alt) in c)
                                       | al <- alRest
                                       | p <- restPs 
                                       | alt <- rest]
          (_:cmpP:testP:tableP:restPs) = scanl mappend (start thisInstr<>c<>c') codes
          posAddr (BC ~(_,a,_)) = verbAddress >§ \va -> toInteger $ va+snd (instrAddress thisInstr)+a
      mapM_ tell codes

compile' (Bind bv arg) = do
  future $ viewing flocations_ $ sequence_ [modify (R.setRan s S.empty) | s <- bindSyms bv]
  viewing locations_ $ sequence_ [modify (R.insert s Memory) | s <- bindSyms bv]
  when (isNothing arg) $ modifying frame_ (frameAlloc defSize bv)
  
compile' Noop = withFuture (align . flocations)
  where align regs = void $ withFreeSet $ loadArgs [(SymVal Value s,Just r) | (s,Register r) <- R.toList regs]

compileOp BCall d (fun:args) = withFreeSet $ do
  let (MemState locs subFrame,_) = defaults [BindVar id (sz,0) 0 []
                                            | (id,arg) <- argAssocs
                                            | sz <- map argSize args] undefined
      argAssocs = zip (map ID [0..]) args
      storeBig top (id,arg) = case lookupAddr id subFrame of
        Just addr -> case argVal arg of
          Left (_,s) -> do
            let loadAddr (r,n) = do a <- stackAddr defSize r ; ld rdi (rsp,fi a,defSize) ; return (rdi,n)
            (base,n) <- maybe ((rsp,) $< stackAddr size s) loadAddr $ binding s
            let addrs = [0,defSize..size]
            sequence_ [ld rax (base,fi $ n+a,fi sz)
                       >> st (rsp,fi $ frameToStack size (top+defSize+addr)+a,fi sz) rax
                      | a <- addrs, let sz = min defSize (size-a)]
            
          Right v -> movi rdi v >> st (rsp,fi $ frameToStack defSize $ top+defSize+addr,defSize) rdi
          where size = argSize arg
        Nothing -> return ()

  modify (SB.delete rax)
  let args' = [(arg,Just r) | (sym,arg) <- argAssocs, Register r <- symLocs sym locs]
  (func:_,cload) <- listen $ loadArgs $ (fun,Nothing):args'

  readFuture $ do
    put (SB.empty compare)
    (_,cstore) <- listen $ storeRegs allocRegs
    top <- lift $ gets (frameTop . frame)
    (_,cstore') <- listen $ do
      lift $ mapM_ (storeBig top) argAssocs
      subri rsp rsp $ Left (fi top)
    let pos = verbAddress >§ \va -> va+snd (instrAddress thisInstr)+delta
        BC ~(_,delta,_) = cload <> cstore <> cstore'
    (call <|||> calli pos) func
    addri rsp rsp $ Left (fi top)
    lift $ modifying locations_ (R.filterRan (not . isRegister))
    lift $ associateVR d rax

compileOp b d [s]
  | b`elem`[BSet,BSetSX,BNot,BSub] && varSize d<=defSize = withFreeSet $ do
    [v] <- loadArgs [(s,Nothing)]
    readFuture $ do
      let dest r | maybe False (not . isActive) (valSym s) = return r
                 | otherwise = destRegister d
      r' <- dest $ (id ||| const 0) v 
      (mov r' <|||> movi r') v
      when (argSize s < varSize d) $ case () of
        () | b`elem`[BSet,BNot] -> zxtnd r' (argSize s)
           | b`elem`[BSetSX,BSub] -> sxtnd r' (argSize s)
      case b of
        BNot -> notr r'
        BSub -> negr r'
        _ -> doNothing
      lift $ associateVar d (S.singleton (Register r'))

compileOp b d [a,a']
  | b`elem`[BAdd,BSub,BMul,BAnd,BOr,BXor] = withFreeSet $ do
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
      lift $ associateVR d dest
  | b`elem`[BLowerThan,BLowerEq,BEqual,BNotEqual,BGreaterEq,BGreaterThan] = withFreeSet $ do
    let flag = fromJust (lookup b codes)
        codes = [(BLowerThan,0xc),(BLowerEq,0xe),(BGreaterEq,0xf),(BGreaterThan,0xd),(BEqual,0x4),(BNotEqual,0x5)]
        applys = [(BLowerThan,(<)),(BLowerEq,(<=)),(BGreaterEq,(>=)),(BGreaterThan,(>)),(BEqual,(==)),(BNotEqual,(/=))]
        convert f n n' = if f n n' then 1 else 0
    [v,v'] <- loadArgs [(a,Nothing),(a',Nothing)]
    readFuture $ do
      opsCode (cmprr,cmpri,cmpir,convert $ fromJust (lookup b applys)) flag v v'
      lift $ associateVL d (Flags flag)
  | b`elem`[BMod,BDiv] && (const False ||| (isJust . log2n ||| const False)) (argVal a') = withFreeSet $ do
    [v,v'] <- loadArgs [(a,Nothing),(a',Nothing)]
    readFuture $ do
      dest <- destRegister d
      let opri BMod dest r (Left n) = bwandri dest r (Left (n-1))
          opri BDiv dest r (Left n) = sari dest r (Left (fi $ fromJust $ log2n n))
      opsCode (undefined,opri b,undefined,if b==BMod then ((.&.) . (subtract 1)) else \n s -> shiftR n (fi s)) dest v v'
      lift $ associateVR d dest
  | b`elem`[BMod,BDiv] = withFreeSet $ do
    [_,_,v] <- loadArgs [(a,Just rax),(IntVal 0,Just rdx),(a',Nothing)]
    readFuture $ do
      case mfilter (/=d) $ valSym a of
        Just s | isActive s -> saveRegs [rax]
        _ -> return ()
      case v of
        Left r -> op [0xf7] 7 7 r
        Right v -> opi (codeFun []) [0xf7] 7 7 v
    lift $ associateVR d (if b==BMod then rdx else rax)
compileOp b d args@(a:a':t) | isBinOp b =
  sequence_ [compileOp b d [a,a']
            | b <- repeat b
            | d <- repeat d
            | a <- a:repeat (SymVal Value d)
            | a' <- a':t]
compileOp _ _ _ = return ()
