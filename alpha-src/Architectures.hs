{-# LANGUAGE CPP #-}
module Architectures (Architecture(..)
                     ,arch_host,arch_x86_64,architectures
                     ,execStub,initStub,callStub0,callStub1) where

import Specialize.Architecture
import qualified Specialize.X86_64 as X86_64
import System.IO.Unsafe (unsafePerformIO)
#if x86_64_HOST_ARCH
import qualified Specialize.X86_64 as Host
#else
#error "Sorry, your architecture is not supported by Alpha just yet :-( "
#endif

architectures = [arch_host,arch_x86_64]
arch_host = Host.arch { archName = "host" }
arch_x86_64 = X86_64.arch

execStub = unsafePerformIO Host.execStub
initStub = unsafePerformIO Host.initStub
callStub0 = unsafePerformIO . Host.callStub0
callStub1 = unsafePerformIO . Host.callStub1
