{-# LANGUAGE ForeignFunctionInterface, NoMonomorphismRestriction, MultiParamTypeClasses #-}
module Context(module Context.Types
              ,module Context.Language
              ,withDefaultContext
              ,languageState
              ,doTransform ,getAddressComp
              ,execCode) where

import System.IO.Unsafe (unsafePerformIO)
import Bindings.Posix.Sys.Mman
import Foreign hiding (unsafePerformIO,unsafeForeignPtrToPtr,void)
import Foreign.C
import Foreign.ForeignPtr.Unsafe
import Data.Maybe
import Data.ByteString.Unsafe
import qualified Data.Map as M
import Data.Functor.Identity
import Data.IORef
import My.Control.Monad
import My.Control.Monad.State

import Context.Types
import Context.Language
import Specialize
import PCode
import ID
import Elf(entryAddress)
import Syntax
import Specialize.Architecture

foreign import ccall "mprotect" mprotect :: Ptr () -> CSize -> CInt -> IO CInt

withRef ref val x = readIORef ref >>= \v -> writeIORef ref val >> x >>= \x -> writeIORef ref v >> return x

contextRef = unsafePerformIO $ newIORef (undefined :: Context)
instance MonadState Context IO where
  get = readIORef contextRef
  put = writeIORef contextRef

addressRef = unsafePerformIO $ newIORef (undefined :: ID -> IO Int)
foreign export ccall "address_" address_ :: ID -> IO Int
address_ i = readIORef addressRef >>= ($i)
foreign import ccall "&address_" address_ptr :: FunPtr (ID -> IO Int)

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
  ("addressC",Right $ castFunPtrToPtr address_ptr)
  ]

doTransform syn = gets transform >>= ($syn)

initialContext = C lang jitA M.empty (fromIntegral entryAddress) return
  where (lang,jitA) = execState (mapM_ st initialBindings) (empty,M.empty)
          where st (s,v) = do
                  i <- stateF fstF (internSym s)
                  case v of
                    Left v -> modifyF fstF (setSymVal i v)
                    Right p -> modifyF sndF (M.insert i $ unsafePerformIO $ newForeignPtr_ p)

withDefaultContext = withState initialContext

contextState sta = (runState sta $< readIORef contextRef) >>= \(a,s') -> writeIORef contextRef s' >> return a
languageState = contextState . doF languageF

foreign import ccall "dynamic" mkProc :: FunPtr (IO ()) -> IO ()
foreign import ccall "dynamic" mkFunSize :: FunPtr (IO Int) -> IO Int
foreign import ccall "dynamic" mkFunInit :: FunPtr (Ptr () -> IO ()) -> Ptr () -> IO ()

getAddress arch lookup register = withRef addressRef getAddr . getAddr
  where
    getAddr id = lookup id >>= \val -> case val of
      Just a -> return a
      Nothing -> gets language >>= \lang -> (>> getAddr id) $ case lookupSymVal id lang of
        Verb c -> void $ do
          let (size,codem) = specialize arch (id,getAddr) c
          ptr <- mallocForeignPtrBytes size
          register id ptr size
          code <- codem
          withForeignPtr ptr $ \p -> do
            unsafeUseAsCStringLen code $ \(p',n) -> copyBytes p (castPtr p') n
            mprotect (castPtr p) (fromIntegral size) (c'PROT_READ .|. c'PROT_WRITE .|. c'PROT_EXEC)
        Noun size init -> do
          size <- join $ evalCode mkFunSize size
          ptr <- mallocForeignPtrBytes size
          register id ptr size
          withForeignPtr ptr $ \p -> evalCode mkFunInit init >>= ($castPtr p)
        _ -> fail $ "Couldn't find definition of symbol "++fromMaybe (show id) (lookupSymName id lang)

evalCode :: (FunPtr f -> f) -> Code -> IO f
evalCode wrap code = do
  id <- languageState $ state createSym >>= \i -> modify (setSymVal i (Verb code)) >> return i
  p <- getAddressJIT id
  return $ wrap $ castPtrToFunPtr $ intPtrToPtr $ fromIntegral p
execCode [] = return ()
execCode instrs = join $ evalCode mkProc (Code [] instrs (symBind (ID (-1))))

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

