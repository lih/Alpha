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

uniquify [] = return []
uniquify code = flatten $< descendM desc M.empty $ spanningTree 0 nexts
  where (_,instr,nexts,_) = navigate code
        desc (instr -> Bind bv v) m = do
          news <- mapM (const $ state createSym) (bindSyms bv)
          let m' = foldr (uncurry M.insert) m (zip (bindSyms bv) news)
          return (Bind (translate (translateBy m') bv) (fmap (translateBy m) v),m')
        desc i m = return (translate (translateBy m) (instr i),m)
        translateBy m s = fromMaybe s $ M.lookup s m

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
      sequence_ [createEdge (mergeEdge t t')  n  n'' | (n  ,t) <- lies, (n'',t') <- loes ]
      sequence_ [deleteEdge                   n  n'  | (n  ,_) <- lies                   ]
      sequence_ [deleteEdge                   n' n'' | (n'',_) <- loes                   ]
      deleteNode n'
    newStart old new = newStart
      where 
        newStart n = 
          maybe (maybe [] (\c -> concat [newStart n | (n,_) <- outEdges c])
                 (lookupContext n old)) 
          (const [n])
          (lookupContext n new)

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
                    (prev,e) = fromMaybe (error $ "Couldn't find edge of "++show n1++" in graph "++show aG) $ find isBackEdge $ inEdges $ getContext n1
            headsEq a b = sort a==sort b
            heads = classesBy eq $ selectHeads $ nodeListFull aG
    tails = map (nub . concatMap saturate) heads
    saturate n = if null nexts then [n] else concatMap saturate nexts
      where nexts = [n' | let c = getContext n                                
                        , (n',c') <- map withContext $ nextNodes c
                        , weight (tag c') > weight (tag c)]
    
    blocks = map blockFromTails tails
    blockFromTails tails = evalState (concat $< mapM makeBlock tails) S.empty
      where 
        visited n = gets (S.member n) ; visit n = modify (S.insert n)
    
        makeBlock n = ifM (visited n) (return []) $ do 
          prevs <- mapM makeBlock (getPrevs n)
          visit n
          return $ concat prevs ++ [n]
        getPrevs n = map fst $ sortBy cmp $ filter p $ map withContext $ prevNodes language
          where cmp (_,c) (_,c') = compare (erNum $ tag c') (erNum $ tag c)
                p (_,c) = weight (tag c) < weight (tag language)
                language = getContext n

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

