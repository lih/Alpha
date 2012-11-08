{-# LANGUAGE NoMonomorphismRestriction #-}
module My.Prelude where

import System.Posix.Files
import Data.Maybe
import Debug.Trace
import My.Control.Monad

debug x = traceShow x x
debugM = liftM debug 

maybeToEither = maybe (Left undefined) Right
eitherToMaybe = either (const Nothing) Just

ifThenElse b th el = if b then th else el

swap (a,b) = (b,a)
thd (_,_,a) = a

lazyPair ~(a,b) = (a,b)

newerThan f1 f2 = liftM2 (>) (modTime f1) (modTime f2)
  where modTime f = modificationTime $< getFileStatus f

