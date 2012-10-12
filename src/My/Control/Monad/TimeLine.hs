{-# LANGUAGE RecursiveDo, NoMonomorphismRestriction #-}
module My.Control.Monad.TimeLine where

import Data.Functor.Identity
import Control.Monad.Fix
import Control.Monad.Trans
import My.Control.Monad
import My.Control.Monad.State

fst3 (a,_,_) = a
snd3 (_,a,_) = a

newtype TimeLineT p f m a = TimeLineT { runTimeLineT :: (p,f) -> m (p,f,a) }
type TimeLine p f = TimeLineT p f Identity

instance MonadFix m => Monad (TimeLineT p f m) where
  tl >>= cc = TimeLineT tl'
    where tl' (p,f) = do rec { (p',f'',a) <- runTimeLineT tl (p,f') ;
                               (p'',f',b) <- runTimeLineT (cc a) (p',f) }
                         return (p'',f'',b)
  return a = TimeLineT (\(p,f) -> return (p,f,a))
instance MonadTrans (TimeLineT p f) where
  lift ma = TimeLineT (\(p,f) -> ma >§ \a -> (p,f,a))
instance (MonadPlus m,MonadFix m) => MonadPlus (TimeLineT p f m) where
  mzero = lift $ mzero
  mplus a b = TimeLineT (\t -> runTimeLineT a t `mplus` runTimeLineT b t)


timeLine tl = TimeLineT (Identity . tl)
runTimeLine tl x = runIdentity $ runTimeLineT tl x

statetl = TimeLineT . \k st -> k st >§ \(a,(p,f)) -> (p,f,a)
runtl = statetl . runStateT

-- Copyright (c) 2012, Coiffier Marc <marc.coiffier@gmail.com>
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

--     Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--     Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DPCodeECT, INDPCodeECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

