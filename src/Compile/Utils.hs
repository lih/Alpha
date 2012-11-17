{-# LANGUAGE TupleSections, ViewPatterns, NoMonomorphismRestriction #-}
module Compile.Utils where

import Compile.State as CS
import qualified My.Data.Graph as G
import My.Prelude
import My.Control.Monad
import My.Control.Monad.State
import My.Data.List
import My.Data.Tree

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Function
import Data.Maybe
import PCode
import Syntax  

import Translate

import Debug.Trace

uniquify a r [] = uniquify a r [ret]
uniquify args ret code = flatten $< descendM uniq (M.fromList $ zip syms syms) $ spanningTree 0 nexts
  where syms = concatMap bindSyms $ maybe id (:) ret args
        (_,instr,nexts,_) = navigate code
        uniq (instr -> Bind bv v) m = do
          news <- mapM (const $ state createSym) (bindSyms bv)
          let m' = foldr (uncurry M.insert) m (zip (bindSyms bv) news)
          return (Bind (translate (translateBy m') bv) (fmap (translateBy m) v),m')
        uniq i m = return (onF fstF (translate (translateBy m)) $ withLocals m $ instr i)
        localVal m (SymVal Value s) | not $ M.member s m = SymVal GValue s
        localVal m v = v
        translateBy m s = fromMaybe s $ M.lookup s m
        withLocals m (Op b v vs) = (Op b v (map (localVal m) vs),M.insert v v m)
        withLocals m (Branch v a) = (Branch (localVal m v) a,m)
        withLocals m i = (i,m)

simplify :: Monad m => [Node] -> StateT CompileState m [Node]
simplify start = do
  oldDep <- getF depGraphF ; purgeAll ; newDep <- getF depGraphF
  return $ concatMap (newStart oldDep newDep) start
  where 
    purgeAll = do
      mapM_ (purgeNode isNoop) =<< getNodeList
      mapM_ (purgeNode isEmptyBranch) =<< getNodeList
    purgeNode p n = do
      c <- getContext n              
      if p c then remove (n,c) else return ()
    isNoop c = tag c==Instr Noop
    isEmptyBranch c = case tag c of 
      BrPart _ -> not (null o) && all p i && all p' o
      _ -> False
      where (i,o) = edges c 
            p  (_,BranchAlt _ _) = True ; p  _ = False
            p' (_,BranchAlt _ 0) = True ; p' _ = False
    remove (n',c) = do
      let 
        (lies,loes) = edges c
        least Forward Forward = Forward   
        least _       _       = Backward  
        mergeEdge (BranchAlt t x) (BranchAlt t' _) = BranchAlt (least t t') x
        mergeEdge TimeDep         b                = b
        mergeEdge e               t                = mergeEdge t e
      sequence_ [deleteEdge                   n  n'  | (n  ,_) <- lies                   ]
      sequence_ [deleteEdge                   n' n'' | (n'',_) <- loes                   ]
      sequence_ [createEdge (mergeEdge t t')  n  n'' | (n  ,t) <- lies, (n'',t') <- loes ]
      deleteNode n'
    newStart old new = newStart
      where 
        newStart n = fromJust $ (lookupContext n new >> return [n])
                     `mplus` (lookupContext n old >§ \c -> concat [newStart n | (n,_) <- outEdges c])
                     `mplus` return []

data ANode = ANode {
  weight :: Int,
  erNum :: Int,
  instr :: NodeData
  }
           deriving Show
linearize start = getsF depGraphF (linearize' start)
linearize' start depG = instrs
  where 
    aG = annotate depG
    getContext n = G.getContext n aG
    withContext n = (n,getContext n)
    (<#) = ((<) `on` (weight . tag))
    
    isBrPart (instr . tag . getContext -> BrPart _) = True
    isBrPart _ = False
    instrMap = M.fromList $ zip instrs (sums (map (length . getInstr) instrs))
      where instrs = concat blocks
    instrs = concatMap (concatMap getInstr) blocks    

    getInstr (getContext -> c) = case tag c of
      ANode { instr = BrPart v } -> [Branch v $ map branch (classesBy (===) oes)]
        where branch ns = minimum $ catMaybes [M.lookup n instrMap | (n,_) <- ns]
              (===) = (==)`on`snd
      ANode { instr = Instr i } -> i : if null oes then [ret] else []
      where oes = outEdges c

    selectHeads l = [n | (n,c) <- l, weight (tag c)==1]
    startHeads = selectHeads $ map withContext $ nub start
    heads = startHeads : deleteBy headsEq startHeads heads
      where eq n1 n2 = if n1`elem`start then n2`elem`start else c'
              where c' = n2 `elem` [n | (n,e') <- outEdges $ getContext prev, e==e']
                    (prev,e) = fromMaybe (error $ "Couldn't find edge of "++show n1++" in graph "++show aG)
                               $ find isBackEdge $ inEdges $ getContext n1
            headsEq a b = sort a==sort b
            heads = classesBy eq $ selectHeads $ nodeListFull aG
    tails = map (S.toList . S.unions) $ classesBy share $ map (S.fromList . concatMap saturate) heads
      where share s s' = not $ S.null (s `S.intersection` s')
    saturate n = if null nexts then [n] else concatMap saturate nexts
      where nexts = [n' | let c = getContext n                                
                        , (n',c') <- map withContext $ nextNodes c
                        , c <# c']

    
    blocks = map blockFromTails tails
    blockFromTails tails = evalState (concat $< mapM makeBlock tails) S.empty
      where 
        visited n = gets (S.member n) ; visit n = modify (S.insert n)
    
        makeBlock n = ifM (visited n) (return []) $ do 
          prevs <- mapM makeBlock (getPrevs n)
          visit n
          return $ concat prevs ++ [n]
        getPrevs n = map fst $ sortBy cmp $ filter p $ map withContext $ prevNodes ctx
          where cmp = compare `on` (erNum . tag . snd)
                p (_,c) = c <# ctx
                ctx = getContext n

annotate depG = newdepG
  where 
    newdepG = mapNodes depCalc depG
    depCalc (depCalc' -> (a,b)) i = ANode a b i
    depCalc' (flip G.getContext depG -> c)
      | any isBackEdge <||> null $ inEdges c = (1,1)
      | otherwise = (1+maximum a, maximum $ zipWith (+) [0..] (reverse $ sort b))
      where (a,b) = unzip [(weight t,erNum t) 
                          | n' <- prevNodes c
                          , let t = tag $ G.getContext n' newdepG]
            
    maximum = foldl max 0

