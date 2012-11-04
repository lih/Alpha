module My.Data.Graph( 
  Graph,Node(..),
  
  empty,
  lookupContext,getContext,
  insertNode,deleteNode,modifyNode,
  insertEdge,deleteEdge,
  
  modifyInEdges,modifyOutEdges,
  tag,inEdges,outEdges,edges,
  nextNodes,prevNodes,
  
  nodeList,nodeListFull,
  
  mapNodes
  )
  where

import qualified Data.Map as M
import Data.List
import Data.Maybe

type Node = Int
data Context e n = Context { 
  nodeTag :: n, 
  nodeInEdges :: M.Map Node e, 
  nodeOutEdges :: M.Map Node e
  } deriving Show
data Graph e n = Graph {
  nbNodes :: Int,
  graphNodes :: M.Map Node (Context e n)
  }
                 
empty = Graph 0 M.empty

lookupContext n (Graph _ nds) = M.lookup n nds
getContext n g = fromMaybe (error $ "No node "++show n++" in graph "++show g) $ lookupContext n g

insertNode x (Graph n nds)       = (n,Graph (n+1) (M.insert n (Context x M.empty M.empty) nds))
deleteNode nd (Graph n nds)      = Graph n (M.delete nd nds)
modifyNode nd f (Graph n nds)    = Graph n (M.adjust (\(Context t i o) -> Context (f t) i o) nd nds)

insertEdge x n1 n2 (Graph n nds) = Graph n (M.adjust (modifyInEdges (M.insert n1 x)) n2 $
                                            M.adjust (modifyOutEdges (M.insert n2 x)) n1 nds) 
deleteEdge n1 n2 (Graph n nds)   = Graph n (M.adjust (modifyInEdges (M.delete n1)) n2 $
                                            M.adjust (modifyOutEdges (M.delete n2)) n1 nds)

modifyInEdges f c = c { nodeInEdges = f $ nodeInEdges c }
modifyOutEdges f c = c { nodeOutEdges = f $ nodeOutEdges c }

tag      = nodeTag
inEdges  = M.toList . nodeInEdges
outEdges = M.toList . nodeOutEdges
edges c  = (inEdges c,outEdges c)

nextNodes = map fst . outEdges
prevNodes = map fst . inEdges

nodeList = map fst . nodeListFull
nodeListFull = M.toList . graphNodes

mapNodes f (Graph n nds) = Graph n (M.mapWithKey (\n (Context t ie oe) -> Context (f n t) ie oe) nds)

instance (Show n,Show e) => Show (Graph e n) where
  show (Graph _ nds) = intercalate "\n" showNodes
    where showNodes = map showNode $ M.toList nds
          showNode (n,Context nd ie oe) = 
            show n++": "++show nd++"\n  "++
            intercalate "\n  " (map showNEdge (M.toList oe)
                                ++ map showREdge (M.toList ie))
              where showNEdge (n',e) = "--"++show e++"--> "++show n'
                    showREdge (n',e) = "<--"++show e++"-- "++show n'


