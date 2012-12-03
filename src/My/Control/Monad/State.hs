{-# LANGUAGE TupleSections, NoMonomorphismRestriction #-}
module My.Control.Monad.State(
  module Control.Monad.State, 
  View(..),
  viewState,viewing,modifying,getting,putting,
  fst_,snd_,id_,f_,on_,
  withState
  ) where

import Prelude hiding ((.),id)
import Control.Monad.State hiding (withState)
import Control.Category

newtype View a v = View (a -> v,v -> a -> a)
instance Category View where
  id = id_
  View (u,u') . View (v,v') = View (u . v, \x a -> v' (u' x (v a)) a)

fst_ = View (fst,(\x ~(a,b) -> (x,b)))
snd_ = View (snd,(\y ~(a,b) -> (a,y)))
id_ = View (id,const)
f_ f = View (f,error "undefined function view")

viewState (View (v,v')) run = state (\s -> let ~(x,s') = run (v s) in (x,v' s' s))
viewing v st                = viewState v (runState st)
modifying v f               = viewState v (\s -> ((),f s))
getting (View (f,_))        = gets f 
putting f v                 = modifying f (const v)

f `on_` View (v,v') = \x -> v' (f (v x)) x

withState s mx = get >>= \v -> put s >> mx >>= \x -> put v >> return x

