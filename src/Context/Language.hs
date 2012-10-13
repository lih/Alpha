{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module Context.Language where

import My.Prelude

import Data.Maybe
import My.Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Bimap as BM

import My.Control.Monad
import My.Control.Monad.State
import Control.Monad.Trans

import ID
import PCode
import Context.Types as C
import Translate

showTable name showLine contents = (name++":"):map ("  "++) (concatMap showLine contents)
instance Show Language where
  show e = intercalate "\n" $ showTable "Context" id [
    ["Max ID: "++show (maxID e)],
    showTable "Symbols" (\(s,i) -> [s++" -> "++show i]) $ BM.toList (symMap e),
    showTable "Aliases" (\(i,i') -> [show i++" -> "++show i']) $ M.toList (aliasMap e),
    showTable "Equivs" (\(i,i') -> [show i++" -> "++show i']) $ M.toList (equivMap e),
    showTable "Modules" (\(s,IDRange r) -> [s++" -> "++show r]) $ BM.toList (modMap e),
    showTable "Values" (\(i,v) -> [show i++" -> "++show v]) $ M.toList (valMap e),
    ["Exports: "++show (exports e)],
    ["Load code: "++show (loadCode e)]]

empty = Language (toEnum 0) BM.empty M.empty M.empty BM.empty M.empty S.empty undefined

createSym e@(Language { maxID = m }) = (m,e { maxID = succ m })
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

envCast t = traverseM (state . intern) t
  where intern "?" = createSym
        intern str = internSym str
              
importLanguage getImport loadImport imp = merge imp
  where 
    merge imp = gets language >>= \l -> unless (imp`isImport`l) $ do
      l' <- getImport imp
      mapM_ merge [imp | (imp,_) <- BM.toList (modMap l')]
      mergeLanguage l'
      loadImport l'
    mergeLanguage l' = doF languageF $ do
      let Language { symMap = syms' , modMap = mods' , maxID = mi' } = l'
      mapM_ (state . internSym) $ BM.keys syms'
      Language { maxID = mi, symMap = syms } <- get
      let aliases = [(i'+mi,fromJust $ BM.lookup s' syms) 
                    | (s',i') <- BM.toList syms']
      modify $ \l -> l {
        maxID = mi+mi',
        aliasMap = aliasMap l `M.union` M.fromList aliases,
        equivMap = equivMap l `M.union` M.fromList (map swap aliases)
        }
      Language { aliasMap = al , modMap = mods } <- get
      let tr s = fromMaybe (tr' s) $ M.lookup (tr' s) al
          tr' s' = fromMaybe (s' + mi) $ do
            m <- lookupSymMod s' l'
            IDRange (r,_) <- BM.lookup m mods
            IDRange (r',_) <- BM.lookup m mods'
            return $ s'-r'+r
          newVals = M.mapKeys tr $ M.map (translate tr) $ valMap l'
      modify $ \l -> l {
        modMap = BM.insert imp (IDRange (mi,mi+mi')) (modMap l),
        valMap = M.unionWith (\_ a -> a) (valMap l) newVals,
        exports = S.difference (exports l) (M.keysSet newVals) 
        }
          
exportLanguage e = e {
  symMap   = BM.filter exportNameP (symMap e),
  valMap   = vals',
  aliasMap = M.empty,
  equivMap = M.empty,
  exports  = S.empty,
  loadCode = translate trans (loadCode e)
  }
  where set2Map s = M.fromAscList (zip (S.toAscList s) (repeat undefined))
        Language { exports = ex, equivMap = eqs } = e
        vals' = M.map (translate trans) $ M.intersection (valMap e) (set2Map ex)
        trans s = fromMaybe s $ M.lookup s eqs
        refs = S.fromList $ concatMap references $ M.elems vals'
        exportNameP _ s = (S.member s ex || S.member s refs)
                          && not (M.member s eqs)
        references (Verb code) = codeRefs code 
        references (Noun size init) = codeRefs size ++ codeRefs init
        references _ = []

-- Copyright (c) 2012, Coiffier Marc <marc.coiffier@gmail.com>
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

--     Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--     Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DPCodeECT, INDPCodeECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

