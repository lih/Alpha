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
    where tl' ~(p,f) = do rec { ~(p',f'',a) <- runTimeLineT tl (p,f') ;
                                ~(p'',f',b) <- runTimeLineT (cc a) (p',f) }
                          return (p'',f'',b)
  return a = TimeLineT (\(~(p,f)) -> return (p,f,a))
instance MonadTrans (TimeLineT p f) where
  lift ma = TimeLineT (\(~(p,f)) -> ma >§ \a -> (p,f,a))
instance (MonadPlus m,MonadFix m) => MonadPlus (TimeLineT p f m) where
  mzero = lift $ mzero
  mplus a b = TimeLineT (\t -> runTimeLineT a t `mplus` runTimeLineT b t)

timeLine tl = TimeLineT (Identity . tl)
runTimeLine tl x = runIdentity $ runTimeLineT tl x

statetl = TimeLineT . \k st -> k st >§ \(~(a,~(p,f))) -> (p,f,a)
runtl = statetl . runStateT
getsp f = runtl $ gets (f . fst)
getPast = runtl $ gets fst
getFuture = runtl $ gets snd
modifyFuture m = runtl $ modifyF sndF m
