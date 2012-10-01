module Environment.Value where

import PCode.Instruction
import PCode.Builtin
import PCode.Value
import Compile.Axiom
import ID
import qualified Data.Set as S
import Data.Tree

data Value = Axiom Axiom
           | Builtin Builtin
           | Verb [BindVar] BindVar [Instruction] 
           | Noun BindVar [Instruction]
           | NoValue
           deriving Show

references val = case val of
  Verb args ret code -> refs code (bindSyms ret++concatMap bindSyms args)
  Noun ret code -> refs code (bindSyms ret)
  _ -> []
  where refs code local = traverse (S.fromList local) (spanningTree 0 nexts)
          where (_,instr,nexts,_) = navigate code
                traverse local (Node i subs) = valRefs (instrVals (instr i)) ++ concatMap (traverse newLocal) subs
                  where valRefs vals = [v | SymVal t v <- vals, t==SymID || not (S.member v local)]
                        newLocal = S.union local (S.fromList (instrVars (instr i)))
                  


-- Copyright (c) 2012, Coiffier Marc <marc.coiffier@gmail.com>
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

--     Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--     Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DPCodeECT, INDPCodeECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

