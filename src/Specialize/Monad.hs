{-# LANGUAGE GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}
module Specialize.Monad(
  module My.Control.Monad.Future,
  module Control.Monad.State.View,
  module My.Control.Monad.WriterAcc,
  Specialize,runSpecialize,tellCode) where

import My.Control.Monad
import Control.Monad.Identity
import My.Control.Monad.Future
import Control.Monad.State.View
import My.Control.Monad.WriterAcc
import Specialize.BinCode
import Specialize.Memory

newtype Specialize a = Specialize (WriterAccT BinCode Position (StateT MemState (FutureT FutureState Identity)) a)
                     deriving (Monad,Functor,Applicative,MonadFix,MonadState MemState
                              ,MonadFuture FutureState,MonadWriter BinCode
                              ,MonadWriterAcc BinCode Position)
runSpecialize (Specialize m) p f = (a,p',f',w)
  where ~(~(~(a,w),p'),f') = runIdentity (runFutureT (runStateT (runWriterAccT m) p) f)

tellCode = tell . bc
