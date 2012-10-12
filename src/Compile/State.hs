{-# LANGUAGE StandaloneDeriving, NoMonomorphismRestriction #-}
module Compile.State(
  module Context, 
  module My.Data.Graph,
  CompileState(..),BranchType(..),EdgeData(..),NodeData(..),CaseInfo(..),
  depGraphF,infoStackF,importsF,
  newVar,
  pushInfo,popInfo,topInfo,withInfo,withTopInfo,
  defaultState,
  getSymName,getSymVal,
  singleCode,
  isBackEdge,
  getNodeList,getLanguage,
  createEdge,deleteEdge,
  createNode,deleteNode,
  nullCode,nullCodeVal,
  makeTimeDep,makeBranch,makeBackBranch,mkNoop,
  (*>>=),(*>>)
  )
  where

import PCode
import My.Control.Monad.State
import My.Control.Monad
import ID
import My.Data.Graph hiding (deleteEdge,deleteNode,getLanguage,Language,empty)
import Context
import Context.Language as L

import qualified My.Data.Graph as G
import qualified Context as E
import qualified Data.Map as M

deriving instance Eq Instruction
deriving instance Eq PCode.Value
deriving instance Eq BindVar

data BranchType = Forward | Backward
                deriving (Show,Eq)
data EdgeData = BranchAlt BranchType Int
              | TimeDep
              deriving (Show,Eq)
data NodeData = Instr Instruction
              | BrPart PCode.Value
              deriving Eq 
type CaseInfo = (Node,[Node],Node,Maybe ID)

data CompileState = CS {
  infoStack :: [CaseInfo],
  imports   :: [String],
  depGraph  :: Graph EdgeData NodeData
  }
                  deriving Show

depGraphF = Field (depGraph,(\g cs -> cs { depGraph = g }))
infoStackF = Field (infoStack,(\l cs -> cs { infoStack = l }))
importsF = Field (imports,(\l cs -> cs { imports = l }))

defaultState = CS [] [] G.empty
singleCode n = ([n],[n])
isBackEdge (_,BranchAlt Backward _) = True
isBackEdge _ = False

getSymName = lift . gets . L.lookupSymName
getSymVal s = lift $ getsF valsF $ M.lookup s
newVar     = lift $ state createSym

pushInfo        = modifyF infoStackF . (:)
popInfo         = stateF infoStackF (\(h:t) -> (h,t))
topInfo         = getsF infoStackF head
withTopInfo i x = pushInfo i >> x >>= \v -> popInfo >> return v
withInfo f      = popInfo >>= \i -> f i >>= \ret -> pushInfo i >> return ret

mkNoop        = createNode (Instr Noop)
nullCode      = nullCodeVal NullVal
nullCodeVal v = mkNoop >>= \n -> return (v,singleCode n)

getNodeList  = getsF depGraphF nodeList
getLanguage n = getsF depGraphF (G.getLanguage n)

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

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DPCodeECT, INDPCodeECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

