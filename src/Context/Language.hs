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
import qualified Data.Traversable as T

import ID
import PCode
import Context.Types as C
import Translate

showTable name showLine contents = (name++":"):map ("  "++) (concatMap showLine contents)
instance Show Language where
  show e = intercalate "\n" $ showTable "Context" id [
    ["Max ID: "++show (maxIDL e)],
    showTable "Symbols" (\(s,i) -> [s++" -> "++show i]) $ BM.toList (symbolsL e),
    showTable "Aliases" (\(i,i') -> [show i++" -> "++show i']) $ M.toList (aliasesL e),
    showTable "Equivs" (\(i,i') -> [show i++" -> "++show i']) $ M.toList (equivsL e),
    showTable "Modules" (\(s,Range r) -> [s++" -> "++show r]) $ BM.toList (languagesL e),
    showTable "Values" (\(i,v) -> [show i++" -> "++show v]) $ M.toList (valuesL e),
    ["Exports: "++show (exportsL e)],
    ["Load code: "++show (initializeL e)]]

empty = Language (toEnum 0) BM.empty M.empty M.empty BM.empty M.empty S.empty []

createSym e@(Language { maxIDL = m }) = (m,e { maxIDL = succ m })
setSymVal id v e = e { valuesL = M.insert id v (valuesL e) }
lookupSymName id e = BM.lookupR id (symbolsL e)
lookupSymVal id e = fromMaybe NoValue $ M.lookup id (valuesL e) 
lookupSymMod id e = BM.lookupR (singleRange id) (languagesL e) 
addExport id e = e { exportsL = S.insert id (exportsL e) }
getImports e = map fst $ BM.toList $ languagesL e
isImport im e = BM.member im (languagesL e)
exportSymVal id v = setSymVal id v . addExport id

internSym s e = runState (st $ BM.lookup s (symbolsL e)) e 
  where st (Just id) = return id
        st _ = do
          i <- state createSym 
          modifyF symsF (BM.insert s i)  
          return i

envCast t = T.mapM (state . intern) t
  where intern "?" = createSym
        intern str = internSym str
              
importLanguage getImport loadImport imp = merge imp
  where 
    merge imp = gets language >>= \l -> unless (imp`isImport`l) $ do
      l' <- getImport imp
      mapM_ merge [imp | (imp,_) <- BM.toList (languagesL l')]
      mergeLanguage l'
      loadImport l'
    mergeLanguage l' = doF languageF $ do
      let Language { symbolsL = syms' , languagesL = mods' , maxIDL = mi' } = l'
      mapM_ (state . internSym) $ BM.keys syms'
      Language { maxIDL = mi, symbolsL = syms } <- get
      let aliases = [(i'+mi,fromJust $ BM.lookup s' syms) 
                    | (s',i') <- BM.toList syms']
      modify $ \l -> l {
        maxIDL = mi+mi',
        aliasesL = aliasesL l `M.union` M.fromList aliases,
        equivsL = equivsL l `M.union` M.fromList (map swap aliases)
        }
      Language { aliasesL = al , languagesL = mods } <- get
      let tr s = fromMaybe (tr' s) $ M.lookup (tr' s) al
          tr' s' = fromMaybe (s' + mi) $ do
            m <- lookupSymMod s' l'
            Range (r,_) <- BM.lookup m mods
            Range (r',_) <- BM.lookup m mods'
            return $ s'-r'+r
          newVals = M.mapKeys tr $ M.map (translate tr) $ valuesL l'
      modify $ \l -> l {
        languagesL = BM.insert imp (Range (mi,mi+mi')) (languagesL l),
        valuesL    = M.unionWith (\_ a -> a) (valuesL l) newVals,
        exportsL   = S.difference (exportsL l) (M.keysSet newVals) 
        }
          
exportLanguage e = e {
  symbolsL    = BM.filter exportNameP (symbolsL e),
  valuesL     = vals',
  aliasesL    = M.empty,
  equivsL     = M.empty,
  exportsL    = S.empty,
  initializeL = translate trans (initializeL e)
  }
  where set2Map s = M.fromAscList (zip (S.toAscList s) (repeat undefined))
        Language { exportsL = ex, equivsL = eqs } = e
        vals' = M.map (translate trans) $ M.intersection (valuesL e) (set2Map ex)
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

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

