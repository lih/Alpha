{-# LANGUAGE TupleSections, ParallelListComp, NoMonomorphismRestriction #-}
module Compile(compile) where

import Compile.State as CS
import Compile.Utils
import Context.Language
import Data.Either
import Data.Maybe
import ID
import My.Control.Monad
import My.Control.Monad.State
import My.Data.Graph as G hiding (deleteEdge,deleteNode)
import My.Data.List
import My.Prelude
import PCode
import Syntax

compile args ret expr = runStateT st defaultState >§ \(code,cs) -> (code,imports cs)
  where st = do
          (_,(start,_)) <- compile' (fmap bindSym ret) expr
          c <- simplify start >>= linearize >>= lift . uniquify args ret
          return $ Code args c ret

compile' dest (Symbol sym) = do
  name <- getSymName sym
  let def = SymVal Value sym
      val = fromMaybe def (IntVal $< (readConstant =<< name))
  fromMaybe (nullCodeVal val) $ do
    v <- dest
    guard (not $ v `varEqVal` val)
    return $ do n <- createNode (Instr $ set v val)
                return (def,singleCode n)
compile' dest (Group (Symbol id:args)) = do
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

compileBuiltin _ dest [] = compileValue dest (IntVal 0)
compileBuiltin b dest args = compileBy (Op b) dest args
compileCall = compileBuiltin BCall

compileAxiom XAlter _ forms = do
  let (vars,exprs) = partitionEithers $ zipWith ($) (cycle [Left,Right]) forms
  codes <- sequence [compile' (Just v) e | Symbol v <- vars | e <- exprs]
  let (starts,ends) = unzip $ map snd codes
  return (NullVal,(concat starts,concat ends))
compileAxiom XBind _ args = case args of
  [bVars] -> doBind bVars Nothing
  [bVars,expr] -> do
    v <- newVar
    compile' (Just v) expr *>> doBind bVars (Just v)
  where
    doBind bVars val = do
      bnd <- bindFromSyntax bVars
      n <- createNode (Instr $ PCode.Bind bnd val)
      return (NullVal,singleCode n)

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
  bv@BindVar { bindSym = sym } <- bindFromSyntax name
  ret <- case bindSubs bv of
    [] -> newVar >>= \ret -> return bv { bindSym = ret }
    (h,_):_ -> return h 
  code <- compileExpr args (Just ret) expr
  lift $ modify $ exportSymVal sym (Verb code)
  compile' dest (Symbol sym)
compileAxiom XVerb dest [Symbol s,Symbol a] = do
  lift $ modify $ \env -> exportSymVal s (lookupSymVal a env) env
  compile' dest (Symbol s)
compileAxiom XNoun dest [Symbol sym,size,init] = do
  v <- newVar
  codeSz <- compileExpr [] (Just $ symBind v) size
  codeInit <- compileExpr [Symbol sym] Nothing init
  lift $ modify $ exportSymVal sym $ Noun codeSz codeInit
  compile' dest (Symbol sym)

compileAxiom XLang _ [Symbol s] = do
  getSymName s >>= maybe (return()) (modify . (\n e -> e { imports = n:imports e }))
  nullCode

compileAxiom XID dest [Symbol s] = compileValue dest (SymVal SymID s)
compileAxiom XAddr dest [Symbol s] = compileValue dest (SymVal Address s)
compileAxiom XSize dest [Symbol s] = compileValue dest (SymVal Size s)

compileAxiom a _ args = error $ "Couldn't compile axiom "++show a++" with args "++show args

compileExpr args ret expr = do
  args <- mapM bindFromSyntax args
  (code,imps) <- lift $ compile args ret expr
  modifyF importsF (imps++)
  return code
compileValue dest val = do
  c <- singleCode $< case dest of
    Just v -> createNode (Instr $ set v val)
    Nothing -> mkNoop
  return (val,c)

bindFromSyntax (Symbol v) = return $ symBind v
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

