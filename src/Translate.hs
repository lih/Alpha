module Translate where

import My.Control.Monad
import Data.Bimap as BM
import ID
import PCode
import Context.Types as C

class Translatable t where
  translate :: (ID -> ID) -> t -> t

instance Translatable ID where
  translate = ($)
instance Translatable IDRange where
  translate t (IDRange (a,b)) = IDRange (t a,t b)
instance Translatable e => Translatable [e] where 
  translate tr l = map (translate tr) l
instance Translatable C.Value where 
  translate tr (Verb code) = Verb (translate tr code)
  translate tr (Noun size init) = Noun (translate tr size) (translate tr init)
  translate tr x = x
instance Translatable Instruction where
  translate tr (Op b v vs) = Op b (translate tr v) (translate tr vs)
  translate tr (Branch v as) = Branch (translate tr v) as
  translate tr (Bind bv v) = Bind (translate tr bv) (translate tr v)
  translate tr Noop = Noop
instance Translatable PCode.Value where
  translate tr (SymVal t id) = SymVal t (translate tr id)
  translate tr v = v
instance Translatable Code where
  translate tr (Code args code ret) = Code (translate tr args) (translate tr code) (translate tr ret)
instance Translatable BindVar where
  translate tr (BindVar id s pad subs) = BindVar (translate tr id) s pad [(translate tr bv,s) | (bv,s) <- subs]

-- Copyright (c) 2012, Coiffier Marc <marc.coiffier@gmail.com>
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

--     Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--     Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DPCodeECT, INDPCodeECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

