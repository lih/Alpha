{-# LANGUAGE ViewPatterns, ParallelListComp #-}
module PCode.Instruction where

import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Array
import My.Data.Tree
import qualified Data.Set as S
import PCode.Builtin
import PCode.Value
import ID
import My.Prelude

type Addr = Int
data BindVar = BindVar { 
  bindSym  :: ID, 
  bindSize :: (Int,Int), 
  bindPad  :: Int, 
  bindSubs :: [(BindVar,Int)]
  }
data Instruction = Op Builtin ID [Value]
                 | Branch Value [Addr]
                 | Bind BindVar (Maybe ID)
                 | Noop
data Code = Code [BindVar] [Instruction] (Maybe BindVar)
            deriving Show

isNoop Noop = True
isNoop _ = False
isBranch (Branch _ _) = True
isBranch _ = False

bindSyms bv = bindSym bv : concatMap bindSyms (map fst $ bindSubs bv)
bindNodes bv = bv:map fst (bindSubs bv)
symBind s = BindVar s (0,1) 0 []
instrVals (Op _ _ vs) = vs
instrVals (Bind _ v) = map (SymVal Value) $ maybeToList v
instrVals (Branch v _) = [v]
instrVals _ = []
instrVars (Op _ v _) = [v]
instrVars (Bind v _) = bindSyms v
instrVars _ = []

flattenBind def = flatten 
  where sizeOf (bindSize -> (s,sr)) = s+sr*def
        flatten bv@(BindVar s _ p subs) = (s,0,sizeOf bv):concat [
           [(s,n+n0,sz) | (s,n,sz) <- flatten bv]
           | (bv,_) <- subs
           | n0 <- scanl (+) p [n*sizeOf bv | (bv,n) <- subs]]
         
set v val = Op BSet v [val]
-- ret = Branch NullVal []
isRet (Branch NullVal []) = True
isRet _ = False

concatCode (Code [] c1 _) (Code [] c2 r) = Code [] (map (f g1) c1 ++ map (f g2) c2) r
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

spanArray bs tree = array bs (assocs Nothing tree)
  where assocs p (Node a subs) = (a,(p,map rootLabel subs)):concatMap (assocs (Just a)) subs
codeRefs (Code args code ret) = [v | SymVal GValue v <- concatMap instrVals code]

instance Show Instruction where  
  show (Op BCall d (f:args)) = show d ++ " = " ++ show f ++ "(" ++ intercalate "," (map show args) ++ ")"
  show (Op BSet v [val]) = show v ++ " = " ++ show val
  show (Op o d vs) = show d ++ " = " ++ intercalate (" "++opStr++" ") (map show vs)
    where opStr = fromMaybe (error $ "unknown builtin "++show o) $ lookup o bNames
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

