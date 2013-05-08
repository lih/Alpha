{-# LANGUAGE TupleSections, ParallelListComp, RecursiveDo, NoMonomorphismRestriction, ViewPatterns #-}
module Compile(compile) where

import Compile.Monad
import Context
import Control.Monad.Writer
import Data.Either
import Data.Ord
import Data.Maybe
import My.Control.Monad
import Control.Monad.State.View
import My.Data.List
import PCode
import qualified Data.Map as M
import Syntax
import Data.Array
import My.Data.Tree

lookupName s = lift $ gets (lookupSymName s)
getSymVal s = lift $ gets (lookupSymVal s)
newVar = lift (state createSym)
intercept m = censor (const mempty) $ listen m
m !- s = fromMaybe s $ M.lookup s m
addLocals ls = modifying locals_ $ \m -> foldr (uncurry M.insert) m ls 
globVal t s locs = case M.lookup s locs of
  Just s' -> SymVal t s'
  Nothing -> SymVal GValue s
branch (IntVal (fromInteger -> n)) alts | n>=0 && n<length (tail alts) = goto (tail alts!!n)
                                        | otherwise = goto (head alts)
branch v alts = tell [Branch v alts] >> return NullVal
goto n = branch NullVal [n]
_a ?>>= b = listen _a >>= \(a,l) -> if null l || not (isBranch $ last l) then b a else return a
a ?>> b = a ?>>= const b
flattenable code = map (f . instr) code'
  where f (Branch v alts) = Branch v (map (a!) alts)
        f i = i
        (_bounds,instr,nexts,_) = navigate code
        t = spanningTree 0 nexts ; code' = flatten t
        a = array _bounds (zip code' [0..])

compile args ret expr = runCompileT (compile' (fmap bindSym ret) expr)
                        (M.fromList [(s,s) | bv <- maybe id (:) ret args, s <- bindSyms bv])
                        <§> \(_,c) -> Code args (flattenable $ c++[mkRetInstr]) ret

compile' dest (Symbol sym) = do
  name <- lookupName sym
  locs <- gets locals
  let def = globVal Value sym locs
      val = fromMaybe def (IntVal <$> (readConstant =<< name))
  case dest of
    Just v | v/=sym -> tell [mkSetInstr v val] >> return def
    _ -> return val
compile' dest (Group []) = compileValue dest (IntVal 0)
compile' dest (Group (verb:args)) = do
  val <- case verb of
    Symbol sym -> Just <$> getSymVal sym
    _ -> return Nothing
  let _compile = case val of
        Just (Axiom a) -> compileAxiom a
        Just (Builtin b) -> compileBuiltin b
        _ -> \d a -> compileCall d (verb:a)
  _compile dest args

schedule dests args = do
  (vals,code) <- unzip <$> sequence [intercept $ compile' dest arg | dest <- dests | arg <- args]
  mapM_ tell (reverse $ sortBy (comparing length) code)
  return vals
                 
compileBy op dest args = do
  vals <- schedule (repeat Nothing) args
  dest' <- maybe newVar return dest
  tell [op dest' vals]
  return (SymVal Value dest')
compileBuiltin b dest args = compileBy (Op b) dest args
compileCall = compileBy (\dest (f:args) -> Call dest f args)

compileAxiom XAlter _ forms = do
  let (vars,exprs) = partitionEithers $ zipWith ($) (cycle [Left,Right]) forms
  locs <- gets locals
  let _assocs = [(v,locs!-v) | Symbol v <- vars]
  schedule (map (Just . snd) _assocs) exprs
  addLocals _assocs
  return NullVal
compileAxiom XBind _ [bVars] = doBind bVars Nothing
compileAxiom XBind _ [bVars,expr] = do
  v <- newVar
  compile' (Just v) expr
  doBind bVars (Just v)
compileAxiom XDo _ [] = return NullVal
compileAxiom XDo dest forms = do
  let cs = reverse $ zipWith compile' (dest:repeat Nothing) (reverse forms)
  last <$> sequence cs
compileAxiom XChoose dest (cond:forms) = mdo
  v <- maybe newVar return dest
       
  pushInfo (start,alts,end,dest)
  start <- getPos
  condVal <- compile' Nothing cond
  branch condVal alts
  let compileAlt alt = saving locals_ $ getPos <* (compile' (Just v) alt ?>> goto end)
  alts <- mapM compileAlt forms
  end <- getPos
  popInfo
           
  return (SymVal Value v)

compileAxiom XReturn _ [arg] = withInfo $ \(_,_,end,dest) -> compile' dest arg ?>> goto end

compileAxiom XRestart _ [] = withInfo $ \(start,_,_,_) -> goto start
compileAxiom XRestart _ [arg] = withInfo $ \(_,alts,_,_) ->
  compile' Nothing arg ?>>= \v -> branch v alts

compileAxiom XAddr dest [Symbol s] = gets locals >>= compileValue dest . globVal Address s
compileAxiom XSize dest [Symbol s] = compileValue dest (SymVal Size s)

compileAxiom XID dest [Symbol s] = compileValue dest (SymVal SymID s)
compileAxiom XVerb dest [Group (name:args),expr] = do
  bv@BindVar { bindSym = sym } <- bindFromSyntax name
  ret <- case bindSubs bv of
    [] -> newVar <§> \ret -> bv { bindSym = ret }
    (h,_):_ -> return h 
  code <- compileExpr args (Just ret) expr
  lift $ modify $ exportSymVal sym (Verb code)
  compile' dest (Symbol sym)
compileAxiom XVerb dest [Symbol s,Symbol a] = do
  lift $ modify $ \env -> exportSymVal s (lookupSymVal a env) env
  compile' dest (Symbol s)
compileAxiom XNoun dest [Symbol sym,size,i] = do
  v <- newVar
  codeSz <- compileExpr [] (Just $ symBind v) size
  codeInit <- compileExpr [Symbol sym] Nothing i
  lift $ modify $ exportSymVal sym $ Noun codeSz codeInit
  compile' dest (Symbol sym)
compileAxiom XLang dest [Symbol sym] = do
  [impSym,idSym] <- lift $ mapM (state . internSym) ["alpha/import","id"]
  compile' dest (Group [Symbol impSym,Group [Symbol idSym,Symbol sym]])

compileAxiom a _ args = error $ "Couldn't compile axiom "++show a++" with args "++show args

compileExpr args ret expr = do
  args' <- mapM bindFromSyntax args
  code <- lift $ compile args' ret expr
  return code
compileValue dest val = (>>return val) $ case dest of
  Just v -> tell [mkSetInstr v val]
  Nothing -> return ()

doBind bVars val = do
  bnd <- bindFromSyntax bVars
  bnd' <- localizeBV bnd
  tell [PCode.Bind bnd' val]
  return NullVal
  where localizeBV (BindVar s sz pad subs) = do
          s' <- newVar
          addLocals [(s,s')]
          subs' <- mapM (\(bv,n) -> liftM (,n) (localizeBV bv)) subs
          return (BindVar s' sz pad subs')
bindFromSyntax (Symbol v) = return $ symBind v
bindFromSyntax (Group (Symbol v:t)) = do
  let fun (ns,l) (Symbol _v) = lookupName _v >>= \s ->
        maybe (fun' ns l (Symbol _v)) (\n -> return (n:ns,l)) (readConstant =<< s)
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
