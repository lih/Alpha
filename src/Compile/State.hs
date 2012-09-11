module Compile.State(
  module Environment, 
  module Util.Graph,
  CompileState(..),BranchType(..),EdgeData(..),NodeData(..),CaseInfo(..),
  envF,depGraphF,infoStackF,
  newVar,
  pushInfo,popInfo,topInfo,withInfo,withTopInfo,
  defaultState,
  getSymName,getSymVal,
  singleCode,
  isBackEdge,
  getNodeList,getContext,
  createEdge,deleteEdge,
  createNode,deleteNode,
  nullCode,nullCodeVal,
  makeTimeDep,makeBranch,makeBackBranch,mkNoop,
  (*>>=),(*>>)
  )
  where

import IR
import Util.State
import Util.Monad
import Util.ID
import Util.Graph hiding (deleteEdge,deleteNode,getContext,Context,empty)
import Environment hiding(lookupSymName)

import qualified Util.Graph as G
import qualified Environment as E
import qualified Data.Map as M

data BranchType = Forward | Backward
                deriving (Show,Eq)
data EdgeData = BranchAlt BranchType Int
              | TimeDep
              deriving (Show,Eq)
data NodeData = Instr Instruction
              | BrPart IR.Value
type CaseInfo = (Node,[Node],Node,Maybe ID)

data CompileState = CS {
  context   :: Context,
  infoStack :: [CaseInfo],
  imports   :: [String],
  depGraph  :: Graph EdgeData NodeData
  }
                  deriving Show

getSymName :: ID -> State CompileState (Maybe String)
getNodeList :: State CompileState [Node]
getContext  :: Node -> State CompileState (G.Context EdgeData NodeData)

envF = (context,\e cs -> cs { context = e })
depGraphF = (depGraph,(\g cs -> cs { depGraph = g }))
infoStackF = (infoStack,(\l cs -> cs { infoStack = l }))

defaultState env = CS env [] [] G.empty
singleCode n = ([n],[n])
isBackEdge (_,BranchAlt Backward _) = True
isBackEdge _ = False

getSymName = getsF envF . E.lookupSymName
getSymVal  = getsF (envF <.> valsF) . M.lookup
newVar     = stateF envF createSym

pushInfo        = modifyF infoStackF . (:)
popInfo         = stateF infoStackF (\(h:t) -> (h,t))
topInfo         = getsF infoStackF head
withTopInfo i x = pushInfo i >> x >>= \v -> popInfo >> return v
withInfo f      = popInfo >>= \i -> f i >>= \ret -> pushInfo i >> return ret

mkNoop        = createNode (Instr Noop)
nullCode      = nullCodeVal NullVal
nullCodeVal v = mkNoop >>= \n -> return (v,singleCode n)

getNodeList  = getsF depGraphF nodeList
getContext n = getsF depGraphF (G.getContext n)

createNode x       = stateF depGraphF (G.insertNode x)
deleteNode n       = modifyF depGraphF (G.deleteNode n)
modifyNode n f     = modifyF depGraphF (G.modifyNode n f)
createEdge x n1 n2 = modifyF depGraphF (G.insertEdge x n1 n2)
deleteEdge n1 n2   = modifyF depGraphF (G.deleteEdge n1 n2)

makeTimeDep a b = do
  (v,(_in,_out)) <- a
  case _out of
    [] -> return (NullVal,(_in,_out))
    _ -> do
      (v,(_in',_out')) <- b v
      sequence_ [createEdge TimeDep n n' | n <- _out,n' <- _in']
      return (v,(_in,_out'))
(*>>=) = makeTimeDep
a *>> b = a *>>= const b

infixr 1 *>>= 
infixr 1 *>> 

makeBranch = makeBranch' Forward
makeBackBranch = makeBranch' Backward
makeBranch' typ val alts = do
  br <- createNode (BrPart val)
  let makeAlt n = createEdge (BranchAlt typ n) br
  case val of
    IntVal i -> makeAlt 0 (if i+1<length alts then tail alts!!i else head alts)
    _        -> sequence_ $ zipWith makeAlt [0..] alts          
  return (NullVal,([br],[]))

instance Show NodeData where
  show (Instr i) = show i
  show (BrPart v) = "case "++show v


-- Copyright (c) 2012, Coiffier Marc <marc.coiffier@gmail.com>
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

--     Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--     Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

