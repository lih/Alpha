{-# LANGUAGE TupleSections, ViewPatterns #-}
module Specialize where

import Specialize.Types
import Specialize.Architecture
import Util.Monad
import Util.State
import Util.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Either
import Data.Array
import Data.Maybe
import Data.Tree

import Environment.Value

import IR

addrF = (addresses,\s p -> p { addresses = s })
bindF = (bindings,\s p -> p { bindings = s })
regsF = (registers,\r p -> p { registers = r })
stackF = (stack,\s p -> p { stack = s })

getAddress id = do
  p <- getp
  case M.lookup id (addresses p) of
    Nothing -> runp $ do
      a <- stateF stackF (stackAlloc (regSize $ architecture $ p))
      modifyF addrF (M.insert id (Nothing,a))
      return a
    Just (_,a) -> return a

shrinkStack s = concatMap f $ tails $ groupBy (\(f1,_) (f2,_) -> not (f1 || f2)) s
  where f [((True,_):_)] = []
        f (l@((True,_):_):_) = if total == 0 then [] else [(True,total)]
          where total = sum [s | (_,s) <- l]
        f ([l]:_) = [l]
stackAlloc n s = (a,shrinkStack s')
  where (h,l) = break (\(free,size) -> free && size>=n) s 
        a = sum $ map snd h
        (size,t) = case l of
          [] -> (n,[])
          ((_,s):t) -> (s,t)
        s' = h++[(False,n),(True,size-n)]++t
stackFree n s = shrinkStack [(i==n || fr,sz) | (i,(fr,sz)) <- zip (scanl (+) 0 (map snd s)) s]

code = [
  Op BSet r [val b],
  Op BSet b [val a],
  Op BNotEqual c [val r,IntVal 0],
  Branch (val c) [6,4],
  Op BSet x [val b],
  Branch NullVal [],
  Op BSet a [val b],
  Op BSet b [val r],
  Op BMod r [val a,val b],
  Branch NullVal [2]  
  ]
  where [a,b,r,c,x] = map var [0..4]
        var = ID
        val v = SymVal Value v

specialize arch code = fmap (\n -> (n,instr n)) (instrTree 0 nexts)
  where (_,instr,nexts,_) = navigate code            

-- Copyright (c) 2012, Coiffier Marc <marc.coiffier@gmail.com>
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

--     Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--     Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

