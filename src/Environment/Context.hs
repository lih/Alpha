module Environment.Context where

import IR
import Data.Maybe
import Environment.Value as E
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Bimap as BM
import Control.Monad.State
import Util.State
import Util.ID
import Util.List
import Util.Monad
import Util.Prelude
import Translate

data Context = CE {
  maxID    :: ID,
  symMap   :: BM.Bimap String ID,
  aliasMap :: M.Map ID ID,
  equivMap :: M.Map ID ID,
  modMap   :: BM.Bimap String IDRange,
  valMap   :: M.Map ID E.Value,
  exports  :: S.Set ID,
  load     :: [Instruction]
  }

showTable name showLine contents = (name++":"):map ("  "++) (concatMap showLine contents)
instance Show Context where
  show e = intercalate "\n" $ showTable "Environment" id [
    ["Max ID: "++show (maxID e)],
    showTable "Symbols" (\(s,i) -> [s++" -> "++show i]) $ BM.toList (symMap e),
    showTable "Aliases" (\(i,i') -> [show i++" -> "++show i']) $ M.toList (aliasMap e),
    showTable "Equivs" (\(i,i') -> [show i++" -> "++show i']) $ M.toList (equivMap e),
    showTable "Modules" (\(s,IDRange r) -> [s++" -> "++show r]) $ BM.toList (modMap e),
    showTable "Values" (\(i,v) -> [show i++" -> "++show v]) $ M.toList (valMap e),
    ["Exports: "++show (exports e)],
    ["Load code: "++show (load e)]]

empty = CE (toEnum 0) BM.empty M.empty M.empty BM.empty M.empty S.empty []

symsF = (symMap,\s ce -> ce { symMap = s })
valsF = (valMap,\v ce -> ce { valMap = v })

createSym e@(CE { maxID = m }) = (m,e { maxID = succ m })
setSymVal id v e = e { valMap = M.insert id v (valMap e) }
lookupSymName id e = BM.lookupR id (symMap e)
lookupSymVal id e = fromMaybe NoValue $ M.lookup id (valMap e) 
lookupSymMod id e = BM.lookupR (singleRange id) (modMap e) 
addExport id e = e { exports = S.insert id (exports e) }
getImports e = map fst $ BM.toList $ modMap e
isImport im e = BM.member im (modMap e)
exportSymVal id v = setSymVal id v . addExport id

internSym s e = runState (st $ BM.lookup s (symMap e)) e 
  where st (Just id) = return id
        st _ = do
          i <- state createSym 
          modifyF symsF (BM.insert s i)  
          return i

envCast t e = runState (traverseM (state . intern) t) e
  where intern "?" = createSym
        intern str = internSym str
              
instance Num ID where
  ID a + ID b = ID (a+b)
  ID a - ID b = ID (a-b)
  (*) = undefined
  negate = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined
  
merge e (mod,e') = if isImport mod e then e else ret
  where ret = execState st e 
        syms' = symMap e' ; mods' = modMap e' ; mi' = maxID e'
        st = do  
          mapM (state . internSym) (BM.keys syms')
          mi <- gets maxID ; syms <- gets symMap
          let aliases = [(i'+mi,fromJust $ BM.lookup s' syms) 
                        | (s',i') <- BM.toList syms']
          modify $ \e -> e {
            maxID = mi+mi',
            aliasMap = aliasMap e `M.union` M.fromList aliases,
            equivMap = equivMap e `M.union` M.fromList (map swap aliases)
            }
          CE { aliasMap = al , modMap = mods } <- get
          let tr s = fromMaybe (tr' s) $ M.lookup (tr' s) al
              tr' s' = fromMaybe (s' + mi) $ do
                m <- lookupSymMod s' e'
                IDRange (r,_) <- BM.lookup m mods
                IDRange (r',_) <- BM.lookup m mods'
                return $ s'-r'+r
              newVals = M.mapKeys tr $ M.map (translate tr) $ valMap e'
          modify $ \e -> e {
            modMap = BM.insert mod (IDRange (mi,mi+mi')) (modMap e),
            valMap = M.unionWith (\_ a -> a) (valMap e) newVals,
            exports = S.difference (exports e) (M.keysSet newVals) 
            }
          
exportContext e = e {
  symMap = BM.filter exportNameP (symMap e),
  valMap = vals',
  aliasMap = M.empty,
  equivMap = M.empty,
  exports = S.empty
  }
  where set2Map s = M.fromAscList (zip (S.toAscList s) (repeat undefined))
        ex = exports e ; eqs = equivMap e
        vals' = M.map (translate tr) $ M.intersection (valMap e) (set2Map ex)
          where tr s = fromMaybe s $ M.lookup s eqs
        refs = S.fromList $ concatMap references $ M.elems vals'
        exportNameP _ s = (S.member s ex || S.member s refs)
                          && not (M.member s eqs)

-- Copyright (c) 2012, Coiffier Marc <marc.coiffier@gmail.com>
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

--     Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--     Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

