{-# LANGUAGE TupleSections #-}

module Util.State(
  module Control.Monad.State, 
  Field(..),(<.>),
  stateF,doF,mapF,modifyF,getF,getsF,putF,swapF,
  fstF,sndF
  ) where

import Control.Monad.State

type Field f s = (s -> f,f -> s -> s)

stateF  :: Field f s -> (f -> (a,f)) -> State s a
mapF    :: Field f s -> ((a,f) -> (b,f)) -> State s a -> State s b
modifyF :: Field f s -> (f -> f) -> State s ()
getF    :: Field f s -> State s f
getsF   :: Field f s -> (f -> a) -> State s a
putF    :: Field f s -> f -> State s ()
swapF   :: Field f s -> f -> State s f
(<.>)   :: Field f s -> Field g f -> Field g s

fstF :: Field a (a,b)
sndF :: Field b (a,b)

fstF = (fst,(\x (a,b) -> (x,b)))
sndF = (snd,(\y (a,b) -> (a,y)))

mapF (m,m') f = mapState (\(v,st) -> let (v',st') = f (v,m st) in (v',m' st' st))
stateF f st   = mapF f (st . snd) (return())
doF f st      = stateF f (runState st)
modifyF fld f = mapF fld (\(v,s) -> (v,f s)) (return ())
getF (f,_)    = gets f 
getsF (f,_) g = gets (g . f)
putF f v      = modifyF f (const v)
swapF f v     = stateF f (,v) 
(f,f') <.> (g,g') = (g . f, (\g s -> f' (g' g (f s)) s)) 

-- Copyright (c) 2012, Coiffier Marc <marc.coiffier@gmail.com>
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

--     Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--     Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

