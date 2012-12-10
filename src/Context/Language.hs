{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module Context.Language where

import My.Prelude

import Data.Maybe
import Data.Monoid
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
    ["Exports: "++show (exportsL e)]]

translateFromTo l l' s = fromMaybe (tr' s) $ M.lookup (tr' s) (aliasesL l)
  where tr' s' = fromMaybe (s' + maxIDL l) $ do
          m <- lookupSymMod s' l'
          Range (r,_) <- BM.lookup m (languagesL l)
          Range (r',_) <- BM.lookup m (languagesL l')
          return $ s'-r'+r
translateEquivs l s = fromMaybe s $ M.lookup s (equivsL l)
instance Monoid Language where
  mempty = Language (error $ "undefined language name") (toEnum 0) BM.empty M.empty M.empty BM.empty M.empty S.empty
  mappend l l' = flip execState l $ do
    mapM_ (state . internSym) $ BM.keys (symbolsL l')
    mi <- gets maxIDL
    modify $ \l ->
      let aliases = [(i'+mi,fromJust $ BM.lookup s' (symbolsL l)) 
                    | (s',i') <- BM.toList (symbolsL l')]
      in l {
        maxIDL = mi + maxIDL l',
        aliasesL = aliasesL l `M.union` M.fromList aliases,
        equivsL = equivsL l `M.union` M.fromList (map swap aliases)
      }
    modify $ \l ->
      let newVals = M.mapKeys tr $ M.map (translate tr) $ valuesL l'
          tr = l `translateFromTo` l'
      in l {
        languagesL = BM.insert (nameL l') (Range (mi,maxIDL l)) (languagesL l),
        valuesL    = M.unionWith (\_ a -> a) (valuesL l) newVals,
        exportsL   = exportsL l S.\\ M.keysSet newVals 
      }

createSym l@(Language { maxIDL = m }) = (m,l { maxIDL = succ m })
setSymVal sym v l = l { valuesL = M.insert sym v (valuesL l) }
lookupSymName sym l = BM.lookupR sym (symbolsL l)
lookupSymVal sym l = fromMaybe NoValue $ M.lookup sym (valuesL l) 
lookupSymMod sym l = BM.lookupR (singleRange sym) (languagesL l) 
addExport sym l = l { exportsL = S.insert sym (exportsL l) }
getImports l = map fst $ BM.toList $ languagesL l
isImport im l = BM.member im (languagesL l)
exportSymVal sym v = setSymVal sym v . addExport sym

internSym s l = runState (st $ BM.lookup s (symbolsL l)) l 
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
      (l',(comp,recomp)) <- tryCompile False imp
      l' <- if not comp && recomp then fst $< tryCompile True imp else return l'
      loadImport =<< mergeLanguage imp l'
      return $ comp || recomp
    mergeLanguage imp (l',init) = viewing language_ $ do
      modify (`mappend`l'{ nameL = imp })
      gets $ \l -> translate (l`translateFromTo`l') init
    tryCompile force imp = do
      (comp,l') <- getImport force imp
      comps <- mapM merge [imp | (imp,_) <- BM.toList (languagesL $ fst l')]
      return (l',(comp,or comps))
