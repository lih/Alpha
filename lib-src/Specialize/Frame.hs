{-# LANGUAGE ViewPatterns #-}
module Specialize.Frame (Frame,frameAlloc,lookupAddr,emptyFrame,frameTop,withAddr) where

import qualified Data.Map as M
import PCode
import ID

data Frame = Frame {
  frameTop :: Int,
  frameAddrs :: M.Map ID Int
  }
           deriving Show

emptyFrame = Frame 0 M.empty

align n a = case n`mod`a of 0 -> n ; m -> n+a-m
frameAlloc sz bv f = Frame (top+(bSize bv`align`sz)) newAddrs
  where bSize (bindSize -> (n,nr)) = n+nr*sz
        newAddrs = foldr (uncurry M.insert) (frameAddrs f) [(s,n+top) | (s,n,_) <- flattenBind sz bv]
        top = frameTop f
lookupAddr s = M.lookup s . frameAddrs
withAddr sz s f = case lookupAddr s f of
  Just a -> (a,f)
  Nothing -> (frameTop f,frameAlloc sz (symBind s) f)
  
