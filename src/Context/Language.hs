{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
-- |A module describing the language system (Alpha's module system)
module Context.Language(
  -- * The Language type (imported from "Context.Types")
  Language(..),translateInit,
  -- * Creating and interning symbols
  createSym,internSym,
  envCast,
  -- * Retrieving information from a Language
  lookupSymName,lookupSymVal,lookupSymMod,
  getImports,isImport,
  -- * Modifying values
  setSymVal,exportSymVal,
  -- * Preparing for serialization
  purgeLanguage
  ) where

import Context.Types as C
import Data.Maybe
import Data.Monoid
import ID
import Control.Monad.State.View
import My.Data.List
import Misc (swap,($^))
import Translate
import qualified Data.Bimap as BM
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Traversable as T

-- |Languages are Monoids, so that importing a Language into another becomes just a matter of appending them !
instance Monoid Language where
  mempty = Language undefined (toEnum 0) BM.empty M.empty M.empty BM.empty M.empty S.empty
  mappend _l l' = execState $^ _l $ do
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
translateFromSub l' l = \s' -> fromMaybe (tr s') $ M.lookup (tr s') (aliasesL l)
  where tr s' = fromMaybe (s' + mi) $ do
          m <- lookupSymMod s' l'
          Range (r,_) <- BM.lookup m (languagesL l)
          Range (r',_) <- BM.lookup m (languagesL l')
          return $ s'-r'+r
        Range (mi,_) = languagesL l BM.! nameL l'
-- |Translates the initialization information from an imported language to the importer.
translateInit c l' l = translate (translateFromSub l' l) c

-- |Creates a new symbol in the Language, and returns it.
createSym l@(Language { maxIDL = m }) = (m,l { maxIDL = succ m })
-- |@interSym s l@ returns the symbol associated to @s@ in @l@. The symbol is created if it doesn't exist.
internSym s l = runState (st $ BM.lookup s (symbolsL l)) l 
  where st (Just sym) = return sym
        st _ = do
          i <- state createSym 
          modifying syms_ (BM.insert s i)  
          return i

-- |Casts a structure of Strings to a structure of symbols, interning every String.
envCast t = T.mapM (state . intern) t
  where intern "?" = createSym
        intern str = internSym str

-- |Returns the name of the given symbol, if it has one.
lookupSymName sym l = BM.lookupR sym (symbolsL l)
-- |Returns the Value of the given symbol. If it has no value, 'NoValue' is returned.
lookupSymVal sym l  = fromMaybe NoValue $ M.lookup sym (valuesL l)
-- |Returns the name of the submodule from which the symbol originates.
lookupSymMod sym l  = BM.lookupR (unitRange sym) (languagesL l) 

-- |Returns a list of all import names of the given Language.
getImports l  = map fst $ BM.toList $ languagesL l
-- |Is a name imported by the Language ?
isImport im l = BM.member im (languagesL l)

-- |Associates the global Value to the given symbol.
setSymVal sym v l  = l { valuesL = M.insert sym v (valuesL l) }
-- |Like 'setSymVal', but also marks the symbol for export.
exportSymVal sym v = setSymVal sym v . addExport
  where addExport l = l { exportsL = S.insert sym (exportsL l) }

-- |Removes all unnecessary information from the given Language, leaving only
-- what is needed for serialization.
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
set2Map s = M.fromAscList (zip (S.toAscList s) (repeat undefined))

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

