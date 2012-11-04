module Specialize.Architecture(Architecture(..),arch_x86,arch_x86_64,arch_arm,hostArch,architectures) where

import Specialize.Types
import Specialize.X86_64

instance Eq Architecture where
  a == a' = archName a == archName a'
instance Show Architecture where
  show a = "#<Arch:"++archName a++">"

undef s = error $ "undefined ("++s++")"

architectures = [hostArch,arch_x86,arch_x86_64,arch_arm]
nullArch = Arch undefined undefined (undef "initials") (undef "compile")
arch_x86 = nullArch { archName = "x86", archDefaultSize = 4 }
arch_arm = nullArch { archName = "arm", archDefaultSize = 4 }

hostArch = arch_x86_64 { archName = "host" }

