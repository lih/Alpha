{-# LANGUAGE ViewPatterns, TupleSections, ParallelListComp, ImplicitParams, NoMonomorphismRestriction, RecursiveDo #-}
module Specialize.X86_64(arch,execStub,initStub,callStub0,callStub1) where

import Control.Arrow
import Control.Monad.Reader
import Control.Monad.Trans
import My.Control.Monad.WriterAcc
import Data.Bits
import Data.Char
import Data.Function as F
import Data.Maybe
import Data.Monoid
import Data.Ord
import ID
import My.Control.Monad
import My.Control.Monad.State
import My.Control.Monad.Future
import My.Data.Either
import My.Data.List
import Misc
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

-- |To delete
test = do
  pre <- getAcc
  debug "ok" `seq` return ()
  debug (posEst pre) `seq` return ()
  debug "ok" `seq` return ()
  
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
argSize = maybe defSize varSize . valSym

frameAddr s = viewState frame_ (withAddr defSize s)
stackAddr sz = liftM (frameToStack sz) . frameAddr
frameToStack sz n = -(n+sz)

associateLoc l s = inLocs $ modify $ s <!- l
associateVar v s = inLocs $ modify $ v -!> s
associateVL v l = inLocs $ modify $ v -!> S.empty >>> S.singleton v <!- l
associateReg = associateLoc . Register
associateVR v = associateVL v . Register

allocReg sym = getFLocs >>= \locs -> do
  let st free = (r,SB.delete r free)
        where r | SB.null free = rdi
                | otherwise = fromMaybe (SB.findMin free) $ mfilter (`SB.member`free) $ symReg sym locs
  state st

destRegister d = pairM (lift getLocs) getFLocs >>= \(locs,flocs) -> 
  case mfilter (\r -> S.null $ S.delete d $ R.lookupDom (Register r) locs) $ symReg d flocs of
    Just r -> return r
    Nothing -> case mfilter isAllocReg (regOf d) `mplus` find (null . symsOf) allocRegs of
      Just r -> return r
      Nothing -> saveRegs [head allocRegs] >> return (head allocRegs)
      where symsOf r = regSyms r locs `mplus` regSyms r flocs
            regOf s = symReg s locs `mplus` symReg s flocs

loadRoot (Just s) = getLocs >>= \locs -> case symReg s locs of
  Just r -> return r
  Nothing -> do
    r <- allocReg s ; a <- lift (stackAddr defSize s)
    ld r (rsp,fi a,defSize)
    lift $ associateReg r (S.singleton s)
    return r
loadRoot Nothing = return rsp

saveRegs rs = lift getLocs >>= \locs -> saveVars $ nubOrd $ concatMap (flip regSyms locs) rs
storeRegs rs = lift getLocs >>= \locs -> storeVars $ nubOrd $ concatMap (flip regSyms locs) rs
saveVars = saveVars' (\p s -> S.size (R.lookupRan s (locations p)) > 1)
storeVars = saveVars' (\p s -> False)
saveVars' isSaved vs = do
  varRegs <- mapM symRegs vs
  let assocs = [(head rs,s,binding s) | s <- vs | rs <- varRegs]
      groups = [(assocRoot (head g),g) | g <- classesBy ((==) `F.on` assocRoot) assocs]
        where assocRoot (_,_,b) = fmap fst b
  mapM_ (storeGroup isSaved) groups
  where 
    storeGroup (root,group) = do
      isSaved <- gets (\p s -> R.member s Memory (locations p) || isSaved p s)
      let loaded = [(isSaved s,a) | a@(_,s,_) <- group]
      when (or [saved | (saved,_) <- loaded]) $ do
        root <- loadRoot root
        mapM_ (\(saved,a) -> unless saved (store root a)) loaded
      where store root (r,s,b) = do
              n <- case b of Just (_,n) -> return n
                             Nothing -> stackAddr (varSize s) s
              st (root,fi n,fi (varSize s)) r
              modifying locations_ (R.insert s Memory)

loadArgs args = do
  let modFlocs = [maybe (S.empty <!- Register r) (-+- Register r) $ valSym arg | (arg,Just r) <- args]
  future $ viewing flocations_ $ mapM_ modify modFlocs
  saveFuture $ do
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
        groups = classesBy ((==)`F.on`parent) assocs
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
  saveFuture $ storeVars $ S.toList $ R.lookupDom Memory locs

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

branchSym (Branch (SymVal Value s) _) = Just s
branchSym _ = Nothing
storeFlags (branchSym -> s) = withFreeSet $ saveFuture $ lift locInfo >>= \(locs,_) -> case R.lookupDom (Flags 0) locs of
  vs | S.null (maybe id S.delete s vs) -> doNothing
     | otherwise -> do
       let s = S.findMin vs
           Flags rf = fromJust (find isFlags $ symLocs s locs)
       r <- destRegister s
       saveRegs [r]
       setcc r rf
       lift $ modifying locations_ $ R.setDom (Register r) vs . R.setDom (Flags 0) S.empty

compile info i = let ?info = info in do
  storeFlags i  
  compile' i
  modifying locations_ (R.filterDom isActive)

compile' (Op b d vs) = do
  future $ modifying flocations_ $ R.setRan d S.empty
  compileOp b d vs
  flip evalStateT (SB.empty compare) $ saveFuture $ do
    (locs,_) <- lift locInfo
    saveVars [s | r <- allocRegs, s <- regSyms r locs, not (isActive s), isJust (binding s)]

compile' (Call d fun args) = withFreeSet $ do
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
  (func:_) <- loadArgs $ (fun,Nothing):args'

  put (SB.empty compare)
  saveFuture $ do
    storeRegs allocRegs
    top <- lift $ gets (frameTop . frame)
    lift $ mapM_ (storeBig top) argAssocs
    subri rsp rsp $ Left (fi top)
    lift $ (call <|||> calli) func
    addri rsp rsp $ Left (fi top)
    lift $ modifying locations_ (R.filterRan (not . isRegister))
    lift $ associateVR d rax

compile' (Branch v alts) = withFreeSet $ do
  let alignPast i = snd $< intercept $ maybe doNothing (\p -> saving id_ $ saveFuture $ do
                                                           lift $ putting frame_ (frame p)
                                                           alignWith $ locations p) (instrPast i)
      jmpc short long (Position (Sum e',Sum s')) = do
        debug e' `seq` return ()
        rec let de = e'-e ; ds = s'-s
                code | de==0 = []
                     | de > -128 && de<=128 = short++take 1 (bytes ds)
                     | otherwise = long++take 4 (bytes ds)
            tell $ BC (length long+4,length code,return $ B.pack code)
            Position (Sum e,Sum s) <- getPos
        return ()
      jmp = jmpc
      altAlign alt = do
        p <- getPos
        (_,a) <- listen (alignPast alt)
        if isEmptyCode a then return (instrPos alt) else jmp (instrPos alt) >> return p
      
  case alts of
    [] -> saveFuture $ do
      p <- lift get
      let isPresent s = or [M.member s (bindings ?info)
                           ,isJust $ lookupAddr s (frame p)
                           ,S.member s (R.domain $ locations p)]
      (_,flocs) <- lift locInfo
      alignWith (R.filterDom isPresent flocs)
      tellCode [0xc3]
    [def] -> alignPast def >> jmp (instrPos def)
    [def,null] -> do
      r <- saveFuture $ do
        rs <- lift $ gets (\p -> fromMaybe [] $ valSym v >§ \s -> symLocs s (locations p))
        case find isFlags rs of
          Just f -> return f
          Nothing -> do
            [Left r] <- loadArgs [(v,Nothing)]
            cmpri r r (Left 0)
            return (Register r)
      let cshort = [0x70+testCode] ; clong = [0x0f,0x80+testCode]
          testCode = oppFlags $ fi $ case r of Flags f -> f ; _ -> 0x4
      rec
        jmpc cshort clong nullPos
        alignPast def >> jmp (instrPos def)
        nullPos <- altAlign null
      return ()
    def:rest -> saveFuture $ do
      [Left r] <- loadArgs [(v,Nothing)]
      cmpri r r (Left $ fi (1+length rest))
      rec
        jmpc [0x72] [0x0f,0x82] tableJump
        alignPast def >> jmp (instrPos def)
        tableJump <- getPos
        movi rsi (Right (64,fi $< absolute $ posAddr table))
        tellCode [fi $ fromFields [(0,1),(r`shiftR`3,1),(0x12,6)]
                 ,0xff,0x24
                 ,fi $ fromFields [(0x6,3),(r.&.7,3),(0x3,2)]]
        table <- getPos
        tell $ fromBytesN (defSize*length rest) (concat $< mapM (liftM (take defSize . bytes) . absolute . posAddr)
                                                 restDsts)
        restDsts <- mapM altAlign rest
      return ()

compile' (Bind bv arg) = do
  future $ viewing flocations_ $ sequence_ [modify (s -!> S.empty) | s <- bindSyms bv]
  viewing locations_ $ sequence_ [modify (s -+- Memory) | s <- bindSyms bv]
  when (isNothing arg) $ modifying frame_ (frameAlloc defSize bv)
  
compile' Noop = withFuture (align . flocations)
  where align regs = void $ withFreeSet $ loadArgs [(SymVal Value s,Just r) | (s,Register r) <- R.toList regs]

calli (either withSize id -> (_,(liftM fi -> fun))) = do
  rec tell $ fromBytesN 5 $ liftM2 (-) fun (absolute $ posAddr pos) >§ \delta -> [0xe8]++take 4 (bytes delta)
      pos <- getPos
  return ()

compileOp b d [s]
  | b`elem`[BSet,BSetSX,BNot,BSub] && varSize d<=defSize = withFreeSet $ do
    [v] <- loadArgs [(s,Nothing)]
    saveFuture $ do
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
    saveFuture $ do
      dest <- destRegister d
      opsCode ops dest v v'
      lift $ associateVR d dest
  | b`elem`[BLowerThan,BLowerEq,BEqual,BNotEqual,BGreaterEq,BGreaterThan] = withFreeSet $ do
    let flag = fromJust (lookup b codes)
        codes = [(BLowerThan,0xc),(BLowerEq,0xe),(BGreaterEq,0xf),(BGreaterThan,0xd),(BEqual,0x4),(BNotEqual,0x5)]
        applys = [(BLowerThan,(<)),(BLowerEq,(<=)),(BGreaterEq,(>=)),(BGreaterThan,(>)),(BEqual,(==)),(BNotEqual,(/=))]
        convert f n n' = if f n n' then 1 else 0
    [v,v'] <- loadArgs [(a,Nothing),(a',Nothing)]
    saveFuture $ do
      opsCode (cmprr,cmpri,cmpir,convert $ fromJust (lookup b applys)) flag v v'
      lift $ associateVL d (Flags flag)
  | b`elem`[BMod,BDiv] && (const False ||| (isJust . log2n ||| const False)) (argVal a') = withFreeSet $ do
    [v,v'] <- loadArgs [(a,Nothing),(a',Nothing)]
    saveFuture $ do
      dest <- destRegister d
      let opri BMod dest r (Left n) = bwandri dest r (Left (n-1))
          opri BDiv dest r (Left n) = sari dest r (Left (fi $ fromJust $ log2n n))
      opsCode (undefined,opri b,undefined,if b==BMod then ((.&.) . (subtract 1)) else \n s -> shiftR n (fi s)) dest v v'
      lift $ associateVR d dest
  | b`elem`[BMod,BDiv] = withFreeSet $ do
    [_,_,v] <- loadArgs [(a,Just rax),(IntVal 0,Just rdx),(a',Nothing)]
    saveFuture $ do
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
