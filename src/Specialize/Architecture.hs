{-# LANGUAGE CPP #-}
module Specialize.Architecture(Architecture(..)
                              ,arch_x86_64,arch_host
                              ,execStub,initStub,callStub0,callStub1
                              ,architectures) where

import Specialize.Types
import qualified Specialize.X86_64 as X86_64
import System.IO.Unsafe (unsafePerformIO)
#if x86_64_HOST_ARCH
import qualified Specialize.X86_64 as Host
#else
#error "Sorry, your architecture is not supported by Alpha just yet :-( "
#endif

instance Eq Architecture where
  a == a' = archName a == archName a'
instance Show Architecture where
  show a = "#<Arch:"++archName a++">"

architectures = [arch_host,arch_x86_64]
arch_host = Host.arch { archName = "host" }
  
arch_x86_64 = X86_64.arch

execStub = unsafePerformIO Host.execStub
initStub = unsafePerformIO Host.initStub
callStub0 = unsafePerformIO . Host.callStub0
callStub1 = unsafePerformIO . Host.callStub1
