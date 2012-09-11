{-# LANGUAGE TupleSections, ParallelListComp #-}
module Compile(compile) where

import Debug.Trace

import Compile.Utils

import Compile.State as CS
import IR
import Util
import Util.ID
import Util.Graph as G hiding (deleteEdge,deleteNode)
import Util.State
import Util.Monad
import Util.List
import Syntax
import Data.Maybe
import Data.Either

compile env dest expr = runState st (defaultState env)
  where st = do
          (_,(start,_)) <- compile' dest expr 
          simplify start >>= linearize
    
compile' dest (Symbol sym) = do
  name <- getSymName sym
  let def = SymVal Value sym
      val = fromMaybe def (IntVal $< (readConstant =<< name))
  fromMaybe (nullCodeVal val) $ do 
    v <- dest
    guard (not $ v `varEqVal` val) 
    return $ do n <- createNode (Instr $ set v val)
                return (def,singleCode n)
compile' dest (Group expr@(Symbol id:args)) = do
  gl <- getSymVal id
  let compile = case gl of
        Just (Axiom a) -> compileAxiom a
        Just (Builtin b) -> compileBuiltin b
        _ -> \d a -> compileCall d (Symbol id:a)
  compile dest args
compile' dest (Group args) = compileCall dest args

compileBy op dest args = do 
  (vars,code) <- unzip $< mapM (compile' Nothing) args
  dest <- maybe newVar return dest
  n <- createNode (Instr $ op dest vars)
  sequence_ [createEdge TimeDep n' n | (_,l) <- code, n' <- l]
  return (SymVal Value dest,(n:concatMap fst code,[n]))

compileBuiltin = compileBy . Op
compileCall = compileBuiltin BCall

compileAxiom XAlter _ forms = do
  let (vars,exprs) = partitionEithers $ zipWith ($) (cycle [Left,Right]) forms
  codes <- sequence [compile' (Just v) e | Symbol v <- vars | e <- exprs] 
  let (starts,ends) = unzip $ map snd codes
  return (NullVal,(concat starts,concat ends))
compileAxiom XBind _ args = doBind args
  where 
    doBind' bVars compile = compile *>>= \v -> do 
      bnd <- bindFromSyntax bVars
      n <- createNode (Instr $ IR.Bind bnd v)
      return (NullVal,singleCode n)
    doBind [bVars] = doBind' bVars $ nullCode
    doBind [bVars,expr] = doBind' bVars $ compile' Nothing expr

compileAxiom XDo dest [] = nullCode
compileAxiom XDo dest forms = do 
  let cs = reverse $ zipWith compile' (dest:repeat Nothing) (reverse forms)
  foldr1 (*>>) cs
compileAxiom XChoose dest (cond:forms) = do
  start <- mkNoop ; end <- mkNoop
  alts  <- replicateM (length forms) mkNoop
  v <- maybe newVar return dest
  let dest = Just v
  
  withTopInfo (start,alts,end,dest) $ do
    return (NullVal,singleCode start) 
      *>> compile' Nothing cond
      *>>= \cv -> do
        let code = zipWith compile' (repeat dest) forms
            fun alt code = return (NullVal,singleCode alt) *>> code *>> makeBranch NullVal [end]
        sequence_ $ zipWith fun alts code                   
        makeBranch cv alts 

  return (SymVal Value v,([start],[end]))

compileAxiom XReturn dest [arg] = withInfo $ \(_,_,end,dest) -> compile' dest arg *>> makeBranch NullVal [end]
            
compileAxiom XRestart _ [] = withInfo $ \(start,_,_,_) -> makeBackBranch NullVal [start]
compileAxiom XRestart _ [arg] = withInfo $ \(_,alts,_,_) -> 
  compile' Nothing arg *>>= \v -> makeBackBranch v alts

compileAxiom XVerb dest [Group (name:args),expr] = do
  bindArgs <- mapM bindFromSyntax args
  (sym,ret,code) <- compileBody name expr
  modifyF envF $ exportSymVal sym (Verb bindArgs ret code)
  compile' dest (Symbol sym)
compileAxiom XVerb dest [Symbol s,Symbol a] = do
  modifyF envF $ \env -> exportSymVal s (lookupSymVal a env) env
  compile' dest (Symbol s)
compileAxiom XNoun dest [name,init] = do
  (sym,ret,code) <- compileBody name init
  modifyF envF $ exportSymVal sym (Noun ret code)
  compile' dest (Symbol sym)
  
compileAxiom XLang _ [Symbol s] = do
  getSymName s >>= maybe (return()) (modify . (\n e -> e { imports = n:imports e }))
  nullCode

compileAxiom XID dest [Symbol s] = compileValue dest (SymVal SymID s)
compileAxiom XAddr dest [Symbol s] = compileValue dest (SymVal Address s)
compileAxiom XSize dest [Symbol s] = compileValue dest (SymVal Size s)

compileAxiom a _ args = error $ "Couldn't compile axiom "++show a++" with args "++show args

compileBody retBind body = do
  ret <- newVar
  bv <- bindFromSyntax retBind
  code <- stateF envF $ \env -> let (c,st) = compile env (Just ret) body in (c,context st)
  return (bindSym bv,bv { bindSym = ret },code)
compileValue dest val = do
  c <- singleCode $< case dest of
    Just v -> createNode (Instr $ set v val)
    Nothing -> mkNoop
  return (val,c)

bindFromSyntax (Symbol v) = return $ BindVar v (0,1) 0 []
bindFromSyntax (Group (Symbol v:t)) = do
  let fun (ns,l) (Symbol v) = getSymName v >>= \s -> 
        maybe (fun' ns l (Symbol v)) (\n -> return (n:ns,l)) (readConstant =<< s)
      fun (ns,l) e = fun' ns l e  
      fun' ns l e = do
        b <- bindFromSyntax e
        return ([],(b,product ns):l)
      
  (pads,subs) <- foldM fun ([],[]) $ reverse t
  let size = foldl (<+>) (pad,0) $ [(n*a,n*b) | (BindVar _ (a,b) _ _,n) <- subs]
      pad = if null pads then 0 else product pads
      (a,b) <+> (a',b') = (a+a',b+b')
  return $ BindVar v size pad subs
bindFromSyntax s = error $ "Invalid shape for bindVar : "++show s

-- Copyright (c) 2012, Coiffier Marc <marc.coiffier@gmail.com>
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

--     Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--     Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

