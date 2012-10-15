{-# LANGUAGE TupleSections, ViewPatterns #-}
module Specialize where

import Specialize.Types
import Specialize.Architecture
import My.Control.Monad
import My.Control.Monad.State
import My.Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Either
import Data.Array
import Data.Maybe
import Data.Tree

import Context.Value

import PCode

addrF = Field (addresses,\s p -> p { addresses = s })
bindF = Field (bindings,\s p -> p { bindings = s })
stackF = Field (stack,\s p -> p { stack = s })

getAddress size id = do
  p <- getp
  case M.lookup id (addresses p) of
    Nothing -> runp $ do
      a <- stateF stackF (stackAlloc size)
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

funArray bs f = array bs [(i,f i) | i <- uncurry enumFromThenTo bs]
specialize arch args code = concatMap snd instructions
  where 
    (bs,instr,nexts,prevs) = navigate code        
    specTree = spanningTree 0 nexts ; specArray = treeArray specTree
    initialPast = archInitialPast arch args
    
    
    instructions = [i | (_,_,i) <- elems instrArray]
    addr i = listArray bs (scanl (\(a,b) (c,d) -> (a+c,b+length d)) (0,0) instructions)!i

    instrTL i ini = do  
      info <- gets $ \a i' -> (addr i'`sub`addr i,let (p,_,_) = a!i in p)
      return $ runTimeLine (compileInstr arch (instr i) info) ini
      where sub (a,b) (c,d) = (a-c,b-d)
    futureOf a = \ns -> case ns of [] -> (Nothing,(initialFuture arch,0),undefined); (x:_) -> a!x
    (instrArray,_) = execState (st (initialPast arch args) specTree) (initial,1)
      where 
        st p (Node i subs) = do
          (a,g) <- get
          let (_,(f,_),_) = futureOf a (map rootLabel subs)
          (p',f',c) <- doF fstF $ instrTL i (p,f) 
          let mark nd = do                        
                fut <- gets futureOf; (p,_,_) <- gets (!nd)
                let cond (fut -> (p,(f,g'),_)) | isJust p || g'>g = Just f
                                               | otherwise = Nothing
                case p >> cond (nexts nd) of  
                  Just f -> do
                    (_,f',_) <- instrTL nd (Nothing,f)
                    modify $ \a -> a//[(nd,(Nothing,(f',g),undefined))]
                    mapM_ mark (prevs nd)
                  Nothing -> return ()
          doF fstF $ mark i
          modify $ \(a,g) -> (a // [(i,(Just p,(f',g),c))],g+1)
          mapM_ (st p') subs
    
    initial = funArray bs f
      where f i = runTimeLine (compileInstr arch (instr i) undefined) (p,f)
              where (parent,subs) = spanArray!i
                    p = maybe (initialPast { bindings = Nothing }) (fst3 . (initial!)) parent
                    f = maybe finalFuture (snd3 . (initial!)) (maybeToList subs)

-- Copyright (c) 2012, Coiffier Marc <marc.coiffier@gmail.com>
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

--     Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--     Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

