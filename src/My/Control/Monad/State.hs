{-# LANGUAGE TupleSections, NoMonomorphismRestriction #-}
module My.Control.Monad.State(
  module Control.Monad.State, 
  Field(..),(<.>),
  stateF,doF,modifyF,getF,getsF,putF,swapF,
  fstF,sndF,
  withState
  ) where

import Control.Monad.State hiding (withState)

newtype Field f s = Field (s -> f,f -> s -> s)

fstF = Field (fst,(\x (a,b) -> (x,b))) :: Field a (a,b)
sndF = Field (snd,(\y (a,b) -> (a,y))) :: Field b (a,b)

stateF (Field (m,m')) st = get >>= \s -> let (v,st') = st (m s) in put (m' st' s) >> return v
doF f st                 = stateF f (runState st)
modifyF fld f            = stateF fld (\s -> ((),f s))
getF (Field (f,_))       = gets f 
getsF (Field (f,_)) g    = gets (g . f)
putF f v                 = modifyF f (const v)
swapF f v                = stateF f (,v) 

Field (f,f') <.> Field (g,g') = Field (g . f, (\g s -> f' (g' g (f s)) s)) 

withState s mx = get >>= \v -> put s >> mx >>= \x -> put v >> return x

-- Copyright (c) 2012, Coiffier Marc <marc.coiffier@gmail.com>
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

--     Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--     Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

