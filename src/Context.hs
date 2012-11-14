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
import Data.ByteString hiding (map,putStrLn)
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

withRef ref val x = readIORef ref >>= \v -> writeIORef ref val >> x >>= \x -> writeIORef ref v >> return x

contextRef = unsafePerformIO $ newIORef (undefined :: Context)
instance MonadState Context IO where
  get = readIORef contextRef
  put = writeIORef contextRef

funPtrToInteger f = fromIntegral $ ptrToIntPtr $ castFunPtrToPtr f

exportAlpha stub ptr = unsafePerformIO $ do
  unsafeUseAsCStringLen (stub $ funPtrToInteger ptr) $ \(src,size) -> do
    ptr <- mallocForeignPtrBytes size
    withForeignPtr ptr $ \dst -> copyBytes (castPtr dst) src size
    return ptr

foreign export ccall "address_" address_ :: ID -> IO Int
foreign import ccall "&address_" address_ptr :: FunPtr (ID -> IO Int)
addressRef = unsafePerformIO $ newIORef (undefined :: ID -> IO Int)
address_ id = readIORef addressRef >>= ($id)

foreign export ccall "symName_" symName_ :: ID -> IO (Ptr Word8)
foreign import ccall "&symName_" symName_ptr :: FunPtr (ID -> IO (Ptr Word8))
symName_ sym = do
  n <- gets (lookupSymName sym . language)
  ret <- newArray0 0 (map c2w $ fromMaybe "" n)
  return ret

foreign export ccall "printOK_" printOK_ :: IO ()
foreign import ccall "&printOK_" printOK_ptr :: FunPtr (IO ())
printOK_ = Prelude.putStrLn "OK"
foreign export ccall "printNum_" printNum_ :: Int -> IO ()
foreign import ccall "&printNum_" printNum_ptr :: FunPtr (Int -> IO ())
printNum_ n = print (intPtrToPtr $ fromIntegral n)

foreign export ccall "doNothing_" doNothing_ :: IO ()
foreign import ccall "&doNothing_" doNothing_ptr :: FunPtr (IO ())
doNothing_ = doNothing

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
  ("addressC"  , Right $ exportAlpha callStub1 address_ptr),    
  ("symName"   , Right $ exportAlpha callStub1 symName_ptr),    
  ("printOK"   , Right $ exportAlpha callStub0 printOK_ptr),    
  ("printNum"  , Right $ exportAlpha callStub1 printNum_ptr),  
  ("doNothing" , Right $ exportAlpha callStub0 doNothing_ptr) 
  ]

doTransform syn = gets transform >>= ($syn)

initialContext = C lang jitA M.empty (fromIntegral entryAddress) return
  where (lang,jitA) = execState (mapM_ st initialBindings) (Lang.empty,M.empty)
          where st (s,v) = do
                  i <- stateF fstF (internSym s)
                  case v of
                    Left v -> modifyF fstF (setSymVal i v)
                    Right p -> modifyF sndF (M.insert i p)

withDefaultContext = withState initialContext

contextState sta = (runState sta $< readIORef contextRef) >>= \(a,s') -> writeIORef contextRef s' >> return a
languageState = contextState . doF languageF

foreign import ccall "dynamic" mkProc :: FunPtr (Ptr() -> IO ()) -> Ptr() -> IO ()
foreign import ccall "dynamic" mkFunSize :: FunPtr (Ptr() -> IO Int) -> Ptr() -> IO Int
foreign import ccall "dynamic" mkFunInit :: FunPtr (Ptr () -> Ptr() -> IO ()) -> Ptr() -> Ptr () -> IO ()

pageSize = fromIntegral $ unsafePerformIO $ c'sysconf c'_SC_PAGESIZE
enableExec p size = do
  let p' = alignPtr (p`plusPtr`(1-pageSize)) pageSize
  mprotect (castPtr p') (fromIntegral $ size+ p`minusPtr`p') (c'PROT_READ .|. c'PROT_WRITE .|. c'PROT_EXEC)
  
getAddress arch lookup register = withRef addressRef getAddr . getAddr
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

evalCode :: (FunPtr (Ptr() -> a) -> Ptr() -> a) -> ByteString -> Code -> (a -> IO b) -> IO b
evalCode wrap stub code f = do
  id <- languageState $ state createSym >>= \i -> modify (setSymVal i (Verb code)) >> return i
  p <- getAddressJIT id
  unsafeUseAsCString stub $ \stub -> f $ wrap (castPtrToFunPtr stub) (intPtrToPtr $ fromIntegral p) 
execCode [] = return ()
execCode instrs = evalCode mkProc execStub (Code [] instrs (symBind (ID (-1)))) id

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

