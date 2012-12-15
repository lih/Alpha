{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module Context.Language where

import Context.Types as C
import Control.Monad.Trans
import Data.Maybe
import Data.Monoid
import ID
import My.Control.Monad
import My.Control.Monad.State
import My.Data.List
import My.Prelude
import PCode
import Translate
import qualified Data.Bimap as BM
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Traversable as T

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

translateInit init l' l = translate (translateFromSub l' l) init
translateFromSub l' l = \s' -> fromMaybe (tr s') $ M.lookup (tr s') (aliasesL l)
  where tr s' = fromMaybe (s' + mi) $ do
          m <- lookupSymMod s' l'
          Range (r,_) <- BM.lookup m (languagesL l)
          Range (r',_) <- BM.lookup m (languagesL l')
          return $ s'-r'+r
        Range (mi,_) = languagesL l BM.! nameL l'
instance Monoid Language where
  mempty = Language undefined (toEnum 0) BM.empty M.empty M.empty BM.empty M.empty S.empty
  mappend l l' = flip execState l $ do
    mapM_ (state . internSym) $ BM.keys (symbolsL l')
    modify $ \l ->
      let aliases = [(i'+mi,symbolsL l BM.! s') 
                    | (s',i') <- BM.toList (symbolsL l')]
          mi = maxIDL l ; nmi = mi + maxIDL l'
      in l { maxIDL = nmi,
             aliasesL = aliasesL l `M.union` M.fromList aliases,
             equivsL = equivsL l `M.union` M.fromList (map swap aliases),
             languagesL = BM.insert (nameL l') (Range (mi,nmi)) (languagesL l) }
    modify $ \l ->
      let newVals = M.mapKeys tr $ M.map (translate tr) $ valuesL l'
          tr = translateFromSub l' l
      in l { valuesL    = M.unionWith (\_ a -> a) (valuesL l) newVals,
             exportsL   = exportsL l S.\\ M.keysSet newVals }

createSym l@(Language { maxIDL = m }) = (m,l { maxIDL = succ m })

lookupSymName sym l = BM.lookupR sym (symbolsL l)
lookupSymVal sym l  = fromMaybe NoValue $ M.lookup sym (valuesL l) 
lookupSymMod sym l  = BM.lookupR (singleRange sym) (languagesL l) 

addExport sym l = l { exportsL = S.insert sym (exportsL l) }

getImports l  = map fst $ BM.toList $ languagesL l
isImport im l = BM.member im (languagesL l)

setSymVal sym v l  = l { valuesL = M.insert sym v (valuesL l) }
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

set2Map s = M.fromAscList (zip (S.toAscList s) (repeat undefined))
purgeLanguage l = mempty {
  maxIDL     = maxIDL l,
  symbolsL   = BM.filter exportNameP (symbolsL l),
  languagesL = languagesL l,
  valuesL    = vals'
  }
  where Language { exportsL = ex, equivsL = eqs } = l
        vals' = M.map (translate $ mapTranslate eqs) $ M.intersection (valuesL l) (set2Map ex)
        refs = S.fromList $ concatMap references (M.elems vals')
        exportNameP _ s = (S.member s ex || S.member s refs) && not (M.member s eqs)

