module Environment.Foreign( 
  getEnv,setEnv,withEnv,stateEnv,stateEnvT,
  defaultEnv,
  doTransform
  )where

import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import Compile.Axiom
import Environment.Value
import Environment.Context
import PCode
import ID
import Syntax
import Control.Monad.State

defaultEnv = fromList $ [
  ("alter"  ,Axiom XAlter),
  ("bind"   ,Axiom XBind),
  
  ("choose" ,Axiom XChoose),
  ("<-"     ,Axiom XRestart),
  ("->"     ,Axiom XReturn),
  ("do"     ,Axiom XDo),
  
  ("lang"   ,Axiom XLang),
  ("verb"   ,Axiom XVerb),
  ("noun"   ,Axiom XNoun),        
            
  ("id"     ,Axiom XID),
  ("@"      ,Axiom XAddr),
  ("#"      ,Axiom XSize)]
  ++ [(n,Builtin b) | (b,n) <- bNames]
  where fromList l = execState (mapM_ st l) empty
        st (s,v) = state (internSym s) >>= \i -> modify (setSymVal i v)

context :: IORef Context
context = unsafePerformIO $ newIORef defaultEnv

getEnv = readIORef context
setEnv = writeIORef context
withEnv e a = do
  e' <- getEnv
  setEnv e
  ret <- a
  setEnv e'
  return ret
stateEnv st = getEnv >>= \e -> let (e',s') = runState st e in setEnv s' >> return e'
stateEnvT st = getEnv >>= \e -> runStateT st e >>= \(e',s') -> setEnv s' >> return e'
doTransform syn = return syn

-- Copyright (c) 2012, Coiffier Marc <marc.coiffier@gmail.com>
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

--     Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--     Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DPCodeECT, INDPCodeECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

