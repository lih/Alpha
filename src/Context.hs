{-# LANGUAGE ForeignFunctionInterface, NoMonomorphismRestriction, MultiParamTypeClasses #-}
module Context(module Context.Types
              ,module Context.Language
              ,withDefaultContext
              ,languageState
              ,doTransform ,getAddressComp
              ,execCode) where

import Bindings.Posix.Unistd
import Bindings.Posix.Sys.Mman
import Context.Language as Lang
import Context.Language
import Context.Types
import qualified Data.ByteString as B
import Data.ByteString.Unsafe
import Data.ByteString.Internal
import Data.Functor.Identity
import Data.IORef
import Data.Maybe
import qualified Data.Map as M
import Elf(entryAddress)
import Foreign.C
import Foreign.ForeignPtr.Unsafe
import Foreign hiding (unsafePerformIO,unsafeForeignPtrToPtr,void)
import ID
import My.Control.Monad
import My.Control.Monad.State
import PCode
import Specialize
import Specialize.Architecture
import Syntax
import System.IO.Unsafe (unsafePerformIO)
import My.Prelude

foreign import ccall "mprotect" mprotect :: Ptr () -> CSize -> CInt -> IO CInt

foreign import ccall "dynamic" mkProc :: FunPtr (Ptr() -> IO ()) -> Ptr() -> IO ()
foreign import ccall "dynamic" mkFunSize :: FunPtr (Ptr() -> IO Int) -> Ptr() -> IO Int
foreign import ccall "dynamic" mkFunInit :: FunPtr (Ptr () -> Ptr() -> IO ()) -> Ptr() -> Ptr () -> IO ()
foreign import ccall "dynamic" mkFunTransform :: FunPtr (Ptr () -> Ptr() -> IO (Ptr ())) -> Ptr() -> Ptr () -> IO (Ptr ())

withRef ref val x = readIORef ref >>= \v -> writeIORef ref val >> x >>= \x -> writeIORef ref v >> return x
funPtrToInteger f = fromIntegral $ ptrToIntPtr $ castFunPtrToPtr f

compAddrRef = unsafePerformIO $ newIORef (undefined :: ID -> IO Int)
contextRef = unsafePerformIO $ newIORef (undefined :: Context)
instance MonadState Context IO where
  get = readIORef contextRef
  put = writeIORef contextRef

exportAlpha stub ptr = unsafePerformIO $ do
  unsafeUseAsCStringLen (stub $ funPtrToInteger ptr) $ \(src,size) -> do
    ptr <- mallocForeignPtrBytes size
    withForeignPtr ptr $ \dst -> copyBytes (castPtr dst) src size
    return ptr

foreign export ccall "setTransform_" setTransform_ :: Ptr () -> IO ()
foreign import ccall "&setTransform_" setTransform_ptr :: FunPtr (Ptr () -> IO ())
setTransform_ fun = modify $ \c -> c { transform = Just fun }

foreign export ccall "compAddr_" compAddr_ :: ID -> IO Int
foreign import ccall "&compAddr_" compAddr_ptr :: FunPtr (ID -> IO Int)
compAddr_ id = readIORef compAddrRef >>= ($id)

foreign export ccall "symName_" symName_ :: ID -> IO (Ptr Word8)
foreign import ccall "&symName_" symName_ptr :: FunPtr (ID -> IO (Ptr Word8))
symName_ sym = do
  n <- gets (lookupSymName sym . language)
  ret <- newArray0 0 (map c2w $ fromMaybe "" n)
  return ret
foreign export ccall "nameSym_" nameSym_ :: Ptr Word8 -> IO ID
foreign import ccall "&nameSym_" nameSym_ptr :: FunPtr (Ptr Word8 -> IO ID)
nameSym_ p = do
  l <- peekArray0 0 p
  stateF languageF (internSym $ map w2c l)
foreign export ccall "createSym_" createSym_ :: IO ID
foreign import ccall "&createSym_" createSym_ptr :: FunPtr (IO ID)
createSym_ = stateF languageF createSym

foreign export ccall "allocate_" allocate_ :: Int -> IO (Ptr ())
foreign import ccall "&allocate_" allocate_ptr :: FunPtr (Int -> IO (Ptr ()))
allocate_ = mallocBytes
foreign export ccall "free_" free_ :: Ptr() -> IO ()
foreign import ccall "&free_" free_ptr :: FunPtr (Ptr() -> IO ())
free_ = free

foreign export ccall "printOK_" printOK_ :: IO ()
foreign import ccall "&printOK_" printOK_ptr :: FunPtr (IO ())
printOK_ = Prelude.putStrLn "OK"
foreign export ccall "printNum_" printNum_ :: Int -> IO ()
foreign import ccall "&printNum_" printNum_ptr :: FunPtr (Int -> IO ())
printNum_ n = print (intPtrToPtr $ fromIntegral n)

initialBindings = [(n,Left $ Builtin b) | (b,n) <- bNames] ++ [
  ("alter"  ,Left $ Axiom XAlter),
  ("bind"   ,Left $ Axiom XBind),

  ("choose" ,Left $ Axiom XChoose),
  ("<-"     ,Left $ Axiom XRestart),
  ("->"     ,Left $ Axiom XReturn),
  ("do"     ,Left $ Axiom XDo),

  ("lang"   ,Left $ Axiom XLang),
  ("verb"   ,Left $ Axiom XVerb),
  ("noun"   ,Left $ Axiom XNoun),

  ("id"     ,Left $ Axiom XID),
  ("@"      ,Left $ Axiom XAddr),
  ("#"      ,Left $ Axiom XSize)] ++ [

  ("alpha/c@"            , Right $ exportAlpha callStub1 compAddr_ptr),    
  ("alpha/create-symbol" , Right $ exportAlpha callStub0 createSym_ptr),
  ("alpha/symbol-name"   , Right $ exportAlpha callStub1 symName_ptr),
  ("alpha/name-symbol"   , Right $ exportAlpha callStub1 nameSym_ptr),

  ("alpha/set-transform" , Right $ exportAlpha callStub1 setTransform_ptr),    

  ("alpha/allocate"      , Right $ exportAlpha callStub1 allocate_ptr), 
  ("alpha/free"          , Right $ exportAlpha callStub1 free_ptr), 
  
  ("alpha/print-OK"      , Right $ exportAlpha callStub0 printOK_ptr),    
  ("alpha/print-num"     , Right $ exportAlpha callStub1 printNum_ptr)

  ]

doTransform syn = gets transform >>= ($syn) . maybe return tr 
  where tr fun tree = do
          root <- allocTree tree
          new <- unsafeUseAsCString initStub $ \stub -> mkFunTransform (castPtrToFunPtr stub) fun root
          readTree new
        intS = sizeOf (undefined::Int) ; ptrS = sizeOf (undefined::Ptr())
        pok e p = poke (castPtr p) e >> return (p`plusPtr`sizeOf e)
        pik p = peek (castPtr p) >>= \e -> return (e,p`plusPtr`sizeOf e)
        allocTree (Group g) = do
          p <- mallocBytes (intS+intS+(length g*ptrS))
          p' <- pok (0::Int) p
          p'' <- pok (length g) p'
          mapM allocTree g >>= \l -> pokeArray (castPtr p'') l
          return p
        allocTree (Symbol (ID s)) = do
          p <- mallocBytes (intS+intS)
          p' <- pok (1::Int) p
          pok s p'
          return p
        readTree p = do
          (t,p') <- pik p
          case t :: Int of
            0 -> do
              (s,p'') <- pik p'
              l <- peekArray s p''
              liftM Group $ mapM readTree l
            1 -> do
              liftM (Symbol . ID) $ peek p'
              
initialContext entry = C lang jitA M.empty (fromIntegral entry) Nothing
  where (lang,jitA) = execState (mapM_ st initialBindings) (Lang.empty,M.empty)
          where st (s,v) = do
                  i <- stateF fstF (internSym s)
                  case v of
                    Left v -> modifyF fstF (setSymVal i v)
                    Right p -> modifyF sndF (M.insert i p)

withDefaultContext = withState . initialContext

contextState sta = (runState sta $< readIORef contextRef) >>= \(a,s') -> writeIORef contextRef s' >> return a
languageState = contextState . doF languageF

pageSize = fromIntegral $ unsafePerformIO $ c'sysconf c'_SC_PAGESIZE
enableExec p size = do
  let p' = alignPtr (p`plusPtr`(1-pageSize)) pageSize
  mprotect (castPtr p') (fromIntegral $ size+ p`minusPtr`p') (c'PROT_READ .|. c'PROT_WRITE .|. c'PROT_EXEC)
  
evalCode :: (FunPtr (Ptr() -> a) -> Ptr() -> a) -> ByteString -> Code -> (a -> IO b) -> IO b
evalCode wrap stub code f = do
  id <- languageState $ state createSym >>= \i -> modify (setSymVal i (Verb code)) >> return i
  p <- getAddressJIT id
  unsafeUseAsCString stub $ \stub -> f $ wrap (castPtrToFunPtr stub) (intPtrToPtr $ fromIntegral p) 
execCode c = evalCode mkProc execStub c id

getAddress arch lookup register = withRef compAddrRef getAddr . getAddr
  where
    getAddr sym = lookup sym >>= \val -> case val of
      Just a -> return a
      Nothing -> gets language >>= \lang -> (>> getAddr sym) $ case lookupSymVal sym lang of
        Verb c -> void $ do
          let (size,codem) = specialize arch (sym,getAddr) c
          ptr <- mallocForeignPtrBytes size
          register sym ptr size
          code <- codem
          withForeignPtr ptr $ \p -> do
            unsafeUseAsCStringLen code $ \(p',n) -> copyBytes p (castPtr p') n
            enableExec p size
        Noun size init -> do
          size <- evalCode mkFunSize execStub size id
          ptr <- mallocForeignPtrBytes size
          register sym ptr size
          withForeignPtr ptr $ \p -> evalCode mkFunInit initStub init ($castPtr p)
        _ -> fail $ "Couldn't find definition of symbol "++fromMaybe (show sym) (lookupSymName sym lang)

getAddressJIT = getAddress hostArch lookup register
  where lookup id = do
          val <- M.lookup id $< gets jitAddresses
          return $ (fromIntegral . ptrToIntPtr . unsafeForeignPtrToPtr) $< val
        register id ptr size = modifyF jitAddressesF (M.insert id ptr)
getAddressComp arch = getAddress arch lookup register
  where lookup id = (fst$<) $< M.lookup id $< gets compAddresses
        register id ptr size = do
          n <- getF compTopF
          modifyF compAddressesF (M.insert id (n,ptr))
          modifyF compTopF (+size)

