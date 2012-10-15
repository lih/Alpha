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

-- Copyright (c) 2012, Coiffier Marc <marc.coiffier@gmail.com>
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

--     Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--     Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

