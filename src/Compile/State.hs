{-# LANGUAGE StandaloneDeriving, NoMonomorphismRestriction, ViewPatterns #-}
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
  getNodeList,getContext,
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
import My.Data.Graph hiding (deleteEdge,deleteNode,getContext,empty)
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
    IntVal (fromIntegral -> i) -> makeAlt 0 (if i+1<length alts then tail alts!!i else head alts)
    _        -> sequence_ $ zipWith makeAlt [0..] alts          
  return (NullVal,([br],[]))

instance Show NodeData where
  show (Instr i) = show i
  show (BrPart v) = "case "++show v
