{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module My.Control.Monad.RWTL(RWTL(..),module Data.Monoid
                            ,fst4,snd4,thd4,fth4
                            ,get,put
                            ,ask,local
                            ,tell,listen,pass,listening
                            ,future,withFuture) where

import Data.Monoid
import Control.Monad.State
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class

newtype RWTL r w p f a = RWTL { runRWTL :: r -> p -> f -> (p,f,a,w) }

fst4 (a,_,_,_) = a
snd4 (_,a,_,_) = a
thd4 (_,_,a,_) = a
fth4 (_,_,_,a) = a

instance Monoid w => Monad (RWTL r w p f) where
  tl >>= cc = RWTL tl'
    where tl' r p f = (p'',f'',b,w`mappend`w')
            where (p',f'',a,w) = runRWTL tl r p f'
                  (p'',f',b,w') = runRWTL (cc a) r p' f
  return a = RWTL (\r p f -> (p,f,a,mempty))
-- the Monoid constraint is not necesary if you want to be precise,
-- but the alternative to liftM would be extremely ugly
instance Monoid w => Functor (RWTL r w p f) where
  fmap = liftM

class MonadFuture f m | m -> f where
  future :: (StateT f m a) -> m a

instance Monoid w => MonadState p (RWTL r w p f) where
  get = RWTL (\_ p f -> (p,f,p,mempty))
  put p = RWTL (\_ _ f -> (p,f,(),mempty))
instance Monoid w => MonadReader r (RWTL r w p f) where
  ask = RWTL (\r p f -> (p,f,r,mempty))
  local l (RWTL rw) = RWTL (rw . l)
instance Monoid w => MonadWriter w (RWTL r w p f) where
  tell w = RWTL (\_ p f -> (p,f,(),w))
  listen (RWTL rw) = RWTL (\r p f -> let (p',f',a,w) = rw r p f in (p',f',(a,w),w))
  pass (RWTL rw) = RWTL (\r p f -> let (p',f',(a,m),w) = rw r p f in (p',f',a,m w))
instance Monoid w => MonadFuture f (RWTL r w p f) where
  future st = RWTL (\r p f -> let (p',_,~(a,f'),w) = runRWTL (runStateT st f) r p f
                              in (p',f',a,w))

listening m = censor (const mempty) $ do (_,w) <- listen m ; return w
withFuture m = future (do f <- get
                          f' <- lift $ future get
                          ret <- lift $ m f
                          put f'
                          return ret)
