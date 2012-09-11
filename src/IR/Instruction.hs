{-# LANGUAGE ViewPatterns #-}
module IR.Instruction where

import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Array
import Data.Tree
import qualified Data.Set as S
import IR.Builtin
import IR.Value
import Util.ID

type Addr = Int
data BindVar = BindVar { 
  bindSym  :: ID, 
  bindSize :: (Int,Int), 
  bindPad  :: Int, 
  bindSubs :: [(BindVar,Int)]
  }
data Instruction = Op Builtin ID [Value]
                 | Branch Value [Addr]
                 | Bind BindVar Value
                 | Noop
                                      
isNoop Noop = True
isNoop _ = False
isBranch (Branch _ _) = True
isBranch _ = False

bindSyms bv = bindSym bv : concatMap bindSyms (map fst $ bindSubs bv)
instrVals (Op _ _ vs) = vs
instrVals (Bind _ v) = [v]
instrVals (Branch v _) = [v]
instrVals _ = []
instrVars (Op _ v _) = [v]
instrVars (Bind v _) = bindSyms v
instrVars _ = []

set v val = Op BSet v [val]
call v f args = Op BCall v (f:args)
ret = Branch NullVal []

concatCode c1 [] = c1
concatCode c1 c2 = map (f g1) c1 ++ map (f g2) c2
  where f g (Branch v as) = Branch v (g as)
        f _ i = i
        g1 as = if null as then [l] else as
        g2 as = map (l+) as
        l = length c1

navigate code = (bs,instr,nexts,prevs)
  where bs@(bMin,bMax) = (0,length code-1)
        instr n = listArray bs code!n
        nexts (instr -> Branch _ l) = l
        nexts n = [n+1]
        prevs n = accumArray (flip (:)) [] bs [(n,i) | i <- [bMin..bMax], n <- nexts i] ! n

instrTree seed nexts = evalState (unfoldTreeM unfold seed) S.empty
  where unfold seed = do modify (S.insert seed) ; s <- get 
                         return (seed,[n | n <- nexts seed, not $ S.member n s])
            
instance Show Instruction where  
  show (Op BCall d (f:args)) = show d ++ " = " ++ show f ++ "(" ++ intercalate "," (map show args) ++ ")"
  show (Op BSet v [val]) = show v ++ " = " ++ show val
  show (Op o d vs) = show d ++ " = " ++ intercalate (" "++opStr++" ") (map show vs)
    where opStr = fromMaybe (error $ "unknown pervasive "++show o) $ lookup o bNames
  show (Branch _ []) = "return"
  show (Branch _ [off]) = "goto " ++ show off
  show (Branch v off) = "case " ++ show v ++ " " ++ show off 
  show (Bind vars x) = "bind "++show vars++" "++show x
  show Noop = "noop"
instance Show BindVar where
  show (BindVar v (n,nr) pad subs) = case (pad,subs) of
    (0,[]) -> head varStr
    _ -> "["++intercalate " " (varStr++padStr++subsStr)++"]"
    where varStr = [show v++":"++if sizeStr=="" then "0" else sizeStr]
          sizeStr = intercalate "+" (size++regSize)
          size = if n==0 then [] else [show n]
          regSize = if nr==0 then [] else [show nr++"r"]
          padStr = if pad==0 then [] else [show pad]
          subsStr = map show subs

-- Copyright (c) 2012, Coiffier Marc <marc.coiffier@gmail.com>
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

--     Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--     Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

