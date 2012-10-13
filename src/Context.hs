{-# LANGUAGE ForeignFunctionInterface, NoMonomorphismRestriction, MultiParamTypeClasses #-}
module Context(module Context.Types
              ,module Context.Language
              ,withDefaultContext
              ,languageState
              ,doTransform ,getAddressComp
              ,execCode) where

import System.IO.Unsafe (unsafePerformIO)
import Bindings.Posix.Sys.Mman
import Foreign hiding (unsafePerformIO,unsafeForeignPtrToPtr)
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

initialContext = C lang jitA M.empty 0 return
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
evalCode :: (FunPtr f -> f) -> Code -> IO f
evalCode wrap code = do 
  binary <- snd $ specialize hostArch getAddressJIT code
  putStrLn $ "Evaluating code "++show code
  unsafeUseAsCStringLen binary $ \(p,s) -> do 
    mprotect (castPtr p) (fromIntegral s) (c'PROT_READ .|. c'PROT_WRITE .|. c'PROT_EXEC)
    return $ wrap $ castPtrToFunPtr p
execCode [] = return ()
execCode instrs = do
  id <- languageState $ state createSym                
  join $ evalCode mkProc (Code [] instrs (symBind id))

getAddress arch lookup register = withRef addressRef getAddr . getAddr
  where 
    getAddr id = lookup id >>= \val -> case val of
      Just a -> return a
      Nothing -> gets language >>= \lang -> (>> getAddr id) $ case lookupSymVal id lang of
        Verb c -> do
          let (size,codem) = specialize arch getAddr c
          ptr <- mallocForeignPtrBytes size
          register id ptr size 
          code <- codem
          withForeignPtr ptr $ \p' -> unsafeUseAsCStringLen code $ \(p,n) -> copyBytes p' (castPtr p) n
        Noun size init -> do
          size <- join $ evalCode mkFunSize size
          ptr <- mallocForeignPtrBytes size
          register id ptr size
          withForeignPtr ptr $ \p -> evalCode mkFunInit init >>= ($castPtr p)
        _ -> fail $ "Couldn't find definition of symbol "++fromMaybe (show id) (lookupSymName id lang)
    
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

-- Copyright (c) 2012, Coiffier Marc <marc.coiffier@gmail.com>
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

--     Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--     Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DPCodeECT, INDPCodeECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

