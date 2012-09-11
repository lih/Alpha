module Util.TimeLine where

import Util.Monad
import Util.State

newtype TimeLine p f a = TimeLine { runTimeLine :: (p,f) -> [(p,f,a)] }

instance Monad (TimeLine p f) where
  tl >>= cc = TimeLine tl'
    where tl' (p,f) = map snd lb
            where la = runTimeLine tl (p,f')
                  lb@((f',_):_) = [(f',(p'',f'',b))
                                  | (p',f'',a) <- la
                                  , (p'',f',b) <- runTimeLine (cc a) (p',f)]
  return a = TimeLine (\(p,f) -> [(p,f,a)])

fork = TimeLine (\(p,f) -> [(p,f,True),(p,f,False)])

statetl = TimeLine . \k st -> let (a,(p,f)) = k st in [(p,f,a)]
runtl = statetl . runState

statep = runtl . stateF fstF
runp = statep . runState
getp = runp get
getsp = runp . gets
modifyp = runp . modify
setp = modifyp . const

statef = runtl . stateF sndF
runf = statef . runState
getf = runf get
getsf = runf . gets
modifyf = runf . modify
setf = modifyf . const

(swapplytl,swapplyp,swapplyf) = (f statetl,f statep,f statef)
  where f st m = st (\x -> (x,m x))

-- Copyright (c) 2012, Coiffier Marc <marc.coiffier@gmail.com>
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

--     Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--     Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

