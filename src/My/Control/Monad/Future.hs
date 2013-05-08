{-# LANGUAGE RecursiveDo, FlexibleInstances, MultiParamTypeClasses, NoMonomorphismRestriction, FunctionalDependencies, UndecidableInstances #-}
-- |A module that describe a Monad in which the state travels backward.
module My.Control.Monad.Future where

import Control.Arrow (first)
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.State.View

-- |A class which allows a state @f@ to travel from the end
-- to the beginning (like a reverse State monad)
class MonadFix m => MonadFuture f m | m -> f where
  {-|
Allows one to operate on the future state.

Implementations should satisfy the following law:

> (future get >>= \x -> future (put y) >> return x) == return y

In other words, modifications to the future state should impact on
the past.
-}
  future :: (State f a) -> m a

instance MonadFuture f m => MonadFuture f (StateT s m) where
  future = lift . future

-- |A canonical implementation of a Future monad transformer.
-- Very similar to a StateT implementation, except the state
-- travels in the opposite direction (but typewise, it is the same)
newtype FutureT f m a = FutureT {
  -- |Runs the Future monad
  runFutureT :: f -> m (a,f)
  }

instance Functor m => Functor (FutureT f m) where
  fmap f (FutureT run) = FutureT (fmap (first f) . run)
instance MonadFix m => Monad (FutureT f m) where
  fm >>= cc = FutureT fm'
    where fm' f = mdo
            ~(a,f'') <- runFutureT fm f'
            ~(b,f') <- runFutureT (cc a) f
            return (b,f'')
  return a = FutureT (\f -> return (a,f))
instance MonadFix m => MonadFuture f (FutureT f m) where
  future st = FutureT (return . runState st)
instance MonadFix m => MonadFix (FutureT f m) where
  mfix fun = FutureT (\f -> mfix (\ ~(a,_) -> runFutureT (fun a) f))

-- |@futureDo st@ executes the State monad @st@, passing it
-- the initial future as initial state and injecting its final state
-- back to the Future.
futureDo st = mdo
  future (put f')
  (a,f') <- runStateT st f
  f <- future get
  return a

-- |A function that executes its argument, passing it its initial future.
withFuture fm = futureDo $ get >>= lift . fm

-- |@saveFuture m@ executes @m@ without modifying the future
saveFuture m = futureDo $ saving id_ (lift m)
