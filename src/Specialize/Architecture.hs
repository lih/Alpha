module Specialize.Architecture(Architecture(..),arch_x86,arch_x86_64,arch_arm,hostArch,architectures) where

import Specialize.Types
import IR.Instruction
import Util.TimeLine
import Data.Ord
import Data.Word
import Data.Set as S

instance Eq Architecture where
  a == a' = archName a == archName a'
instance Show Architecture where
  show a = "#<Arch:"++archName a++">"

architectures = [hostArch,arch_x86,arch_x86_64,arch_arm]
arch_x86 = Arch {
  archName = "x86",
  registers = S.fromList [0..7],
  regSize = 4,
  compileInstr = undefined
  }
arch_x86_64 = Arch { 
  archName = "x86_64",
  registers = S.fromList [0..15],
  regSize = 8,
  compileInstr = undefined
  }
arch_arm = Arch {
  archName = "arm",
  registers = S.fromList [0..15],
  regSize = 4,
  compileInstr = undefined
  }
hostArch = arch_x86_64 { archName = "host" }

-- Copyright (c) 2012, Coiffier Marc <marc.coiffier@gmail.com>
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

--     Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--     Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

