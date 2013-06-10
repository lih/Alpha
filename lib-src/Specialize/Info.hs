{-# LANGUAGE ImplicitParams, NoMonomorphismRestriction #-}
module Specialize.Info(
  -- * The Info type
  Info(..),
  -- * Variable information
  binding,isActiveB,isActiveA,varSize,argSize,bSize,
  -- * Instruction information
  instrPos,instrPast,thisInstr,
  -- * Global position information
  globAddress,verbAddress,absolute
  ) where

import ID
import Data.Maybe
import Data.Map as M
import Data.Set as S
import My.Data.Relation as R
import Specialize.Memory
import Specialize.BinCode (mkPos)
import Control.Monad (liftM)
import PCode

-- |A type describing compile information for a given 'Instruction'
data Info = Info {
  envInfo    :: (ID,ID -> IO Int),
  bindings   :: Map ID (ID,Int),
  sizesI     :: Map ID Int,
  actives    :: (Set ID,Set ID),
  clobbersI  :: Relation ID ID,
  branchPos  :: (Int,Int -> (Int,Int,Maybe MemState))
  }

instance Show Info where
  show (Info _ b sz a c _) = "Info { bindings = "++show b
                               ++", sizes = "++show sz
                               ++", actives = "++show a
                               ++", clobbers = "++show c
                               ++" }"

-- |The (possible) binding of the given variable, of the form @(root,offset)@
binding s = M.lookup s (bindings ?info)
-- |Is the variable active before the instruction ?
isActiveB s = S.member s (fst $ actives ?info)
-- |Is the variable active after the instruction ?
isActiveA s = S.member s (snd $ actives ?info)
-- |The size of a variable
varSize s = fromMaybe ?defaultSize $ M.lookup s (sizesI ?info)
-- |The size of an argument
argSize (SymVal Value s) = varSize s
argSize _ = ?defaultSize
-- |The size of a bindVar
bSize BindVar { bindSize = (n,nr) } = n+nr* ?defaultSize

instrInfo = snd (branchPos ?info)
-- |The position of the given instruction, relative to the beginning of the verb
instrPos i = let (e,s,_) = instrInfo i in mkPos e s
-- |The memory state of the beginning of the given instruction, if already specialized
instrPast i = let (_,_,p) = instrInfo i in p 
-- |The index of the current instruction
thisInstr = fst (branchPos ?info)

-- |The absolute address of the current verb
verbAddress = let (me,addrs) = envInfo ?info in addrs me
-- |The absolute address of the given verb
globAddress sym = snd (envInfo ?info) sym
-- |Translates an address relative to the start of the verb to an absolute one 
absolute addr = liftM (+addr) verbAddress
