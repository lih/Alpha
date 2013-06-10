{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, NoMonomorphismRestriction, UndecidableInstances #-}
{-| A module describing a Writer monad with accumulator. -}
module My.Control.Monad.WriterAcc (
  module Control.Monad.Writer,
  -- * Utilities
  intercept,
  -- * The WriterAcc class
  Endomorphic(..),
  MonadWriterAcc(..),
  -- * A WriterAcc Monad implementation
  WriterAccT,runWriterAccT) where

import Control.Applicative
import Control.Monad.Writer
import Control.Monad.RWS
import Control.Monad.State
import My.Control.Monad.Future

-- |@intercept w@ executes @w@ and returns what was written instead of writing it.
intercept = censor (const mempty) . listen

-- |Describes an endomorphism relation between the two monoids @m@ and @m'@.
class (Monoid m,Monoid m') => Endomorphic m m' where
  endo :: m -> m'
  {- ^
Morphism from @m@ to @m'@. The following relations should hold:

> endo mempty = mempty

> endo (a <> b) = endo a <> endo b
-}
instance Endomorphic [a] (Sum Int) where
  endo = Sum . length

-- |An extension of the MonadWriter class in which you may retrieve an accumulator
class (Endomorphic w acc,MonadWriter w m) => MonadWriterAcc w acc m where
  getAcc :: m acc
instance (MonadWriterAcc w acc m) => MonadWriterAcc w acc (StateT s m) where
  getAcc = lift getAcc

{-|The WriterAccT monad transformer. -}
newtype WriterAccT w acc m a = WA { runWA :: RWST () w acc m a }
                             deriving (Functor,Monad,Applicative,MonadFix,MonadTrans)

instance (Monad m,Endomorphic w acc) => MonadWriter w (WriterAccT w acc m) where
  tell w = WA (tell w >> modify (<>endo w))
  listen = WA . listen . runWA
  pass (WA m) = WA $ do
    cur <- get
    (a,w) <- listen (pass m)
    put $ cur<>endo w
    return a
instance (Monad m,Endomorphic w acc) => MonadWriterAcc w acc (WriterAccT w acc m) where
  getAcc = WA get  
instance (MonadState s m,Endomorphic w acc) => MonadState s (WriterAccT w acc m) where
  get = WA (lift get)
  put = WA . lift . put
instance (MonadFuture f m,Endomorphic w acc) => MonadFuture f (WriterAccT w acc m) where
  future = lift . future

-- |Runs a WriterAccT monad, supplying it with an empty accumulator
runWriterAccT (WA m) = evalRWST m () mempty
