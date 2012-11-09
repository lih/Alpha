{-# LANGUAGE TupleSections, NoMonomorphismRestriction #-}
module My.Control.Monad.State(
  module Control.Monad.State, 
  Field(..),(<.>),
  stateF,doF,modifyF,getF,getsF,putF,swapF,
  fstF,sndF,
  withState
  ) where

import Control.Monad.State hiding (withState)

newtype Field f s = Field (s -> f,f -> s -> s)

fstF = Field (fst,(\x ~(a,b) -> (x,b))) :: Field a (a,b)
sndF = Field (snd,(\y ~(a,b) -> (a,y))) :: Field b (a,b)

stateF (Field (m,m')) st = get >>= \s -> let ~(v,st') = st (m s) in put (m' st' s) >> return v
doF f st                 = stateF f (runState st)
modifyF fld f            = stateF fld (\s -> ((),f s))
getF (Field (f,_))       = gets f 
getsF (Field (f,_)) g    = gets (g . f)
putF f v                 = modifyF f (const v)
swapF f v                = stateF f (,v) 

Field (f,f') <.> Field (g,g') = Field (g . f, (\g s -> f' (g' g (f s)) s)) 

withState s mx = get >>= \v -> put s >> mx >>= \x -> put v >> return x

