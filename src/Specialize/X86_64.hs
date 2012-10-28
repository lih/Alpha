module Specialize.X86_64(arch_x86_64) where

import Specialize.Types
import Control.Monad
import qualified Data.ByteString as B

defSize = 8
arch_x86_64 = Arch "x86_64" defSize defaults compile




(e,s,c) <++> (e',s',c') = (e+e',s+s',liftM2 B.append c c')

defaults args ret = undefined
compile = undefined
