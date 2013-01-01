{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}
module My.Control.Monad.WriterAcc (WriterAccT,module W,runWriterAccT,getAcc) where

import Control.Monad.Writer as W
import Control.Monad.RWS

newtype WriterAccT w acc m a = WA { runWA :: RWST () w acc m a }
                             deriving (Monad,MonadFix,MonadTrans)

class Endomorphic m m' where
  endo :: m -> m'
instance Endomorphic [a] (Sum Int) where
  endo = Sum . length

instance (Monad m,Monoid acc,Monoid w,Endomorphic w acc) => MonadWriter w (WriterAccT w acc m) where
  tell w = WA (tell w >> modify (<>endo w))
  listen (WA m) = WA (listen m)
  pass (WA m) = WA $ do
    cur <- get
    (a,w) <- listen (pass m)
    put $ cur<>endo w
    return a

getAcc = WA get
runWriterAccT (WA m) = evalRWST m () mempty
