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

emptyLanguage = Language (toEnum 0) BM.empty M.empty M.empty BM.empty M.empty S.empty []

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
          modifying syms_ (BM.insert s i)  
          return i

envCast t = T.mapM (state . intern) t
  where intern "?" = createSym
        intern str = internSym str
              
importLanguage getImport loadImport imp = merge imp
  where 
    merge imp = gets language >>= \l -> ifThenElse (imp`isImport`l) (return False) $ do
      (comp,l') <- getImport False imp
      comps <- mapM merge [imp | (imp,_) <- BM.toList (languagesL l')]
      let recomp = or comps
      l' <- if not comp && recomp then do
        (_,l') <- getImport True imp
        mapM merge [imp | (imp,_) <- BM.toList (languagesL l')]
        return l'
                else return l'
      init <- mergeLanguage imp l'
      loadImport init
      return $ comp || recomp
    mergeLanguage imp l' = viewing language_ $ do
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
      Language { aliasesL = al , languagesL = mods, initializeL = init } <- get
      let tr s = fromMaybe (tr' s) $ M.lookup (tr' s) al
          tr' s' = fromMaybe (s' + mi) $ do
            m <- lookupSymMod s' l'
            Range (r,_) <- BM.lookup m mods
            Range (r',_) <- BM.lookup m mods'
            return $ s'-r'+r
          newVals = M.mapKeys tr $ M.map (translate tr) $ valuesL l'
      modify $ \l -> l {
        languagesL = BM.insert imp (Range (mi,mi+mi')) mods,
        valuesL    = M.unionWith (\_ a -> a) (valuesL l) newVals,
        exportsL   = exportsL l S.\\ M.keysSet newVals 
        }
      return (translate tr init)
          
exportLanguage l = l {
  symbolsL    = BM.filter exportNameP (symbolsL l),
  valuesL     = vals',
  aliasesL    = M.empty,
  equivsL     = M.empty,
  exportsL    = S.empty,
  initializeL = translate trans (initializeL l)
  }
  where set2Map s = M.fromAscList (zip (S.toAscList s) (repeat undefined))
        Language { exportsL = ex, equivsL = eqs } = l
        vals' = M.map (translate trans) $ M.intersection (valuesL l) (set2Map ex)
        trans s = fromMaybe s $ M.lookup s eqs
        refs = S.fromList $ concatMap references $ M.elems vals'
        exportNameP _ s = (S.member s ex || S.member s refs)
                          && not (M.member s eqs)
        references (Verb code) = codeRefs code 
        references (Noun size init) = codeRefs size ++ codeRefs init
        references _ = []

