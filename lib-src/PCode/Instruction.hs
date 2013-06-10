{-# LANGUAGE ViewPatterns, ParallelListComp #-}
module PCode.Instruction where

import Data.List
import Data.Maybe
import Data.Array
import My.Data.Tree
import PCode.Builtin
import PCode.Value
import ID

type Addr = Int
data BindVar = BindVar { 
  bindSym  :: ID, 
  bindSize :: (Int,Int), 
  bindPad  :: Int, 
  bindSubs :: [(BindVar,Int)]
  }
data Instruction = Op Builtin ID [Value]
                 | Call ID Value [Value]
                 | Branch Value [Addr]
                 | Bind BindVar (Maybe ID)
                 | Noop
data Code = Code [BindVar] [Instruction] (Maybe BindVar)
            deriving Show

isNoop Noop = True
isNoop _ = False
isBranch (Branch _ _) = True
isBranch _ = False
isRet (Branch NullVal []) = True
isRet _ = False

mkSetInstr v val = Op BSet v [val]
mkRetInstr = Branch NullVal []

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

flattenBind def = _flatten 
  where sizeOf (bindSize -> (s,sr)) = s+sr*def
        _flatten bv@(BindVar s _ p subs) = (s,0,sizeOf bv):concat [
           [(s',n+n0,sz) | (s',n,sz) <- _flatten bv']
           | (bv',_) <- subs
           | n0 <- scanl (+) p [n*sizeOf bv'' | (bv'',n) <- subs]]
         
navigate code = (bs,instr,nexts,prevs)
  where bs@(bMin,bMax) = (0,length code-1)
        instr n = listArray bs code!n
        nexts (instr -> Branch _ l) = l
        nexts n = [n+1]
        prevs n = accumArray (flip (:)) [] bs [(nx,i) | i <- [bMin..bMax], nx <- nexts i] ! n

spanArray bs tree = array bs (_assocs Nothing tree)
  where _assocs p (Node a subs) = (a,(p,map rootLabel subs)):concatMap (_assocs (Just a)) subs
codeRefs (Code _ code _) = [v | SymVal t v <- concatMap instrVals code, t == GValue || t==SymID]

instance Show Instruction where  
  show (Op BSet v [val]) = show (SymVal Value v) ++ " = " ++ show val
  show (Op o d vs) = show (SymVal Value d) ++ " = " ++ intercalate (" "++opStr++" ") (map show vs)
    where opStr = fromMaybe (error $ "unknown builtin "++show o) $ lookup o bNames
  show (Call d f args) = show (SymVal Value d) ++ " = " ++ show f ++ "(" ++ intercalate "," (map show args) ++ ")"
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

