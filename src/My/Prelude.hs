{-# LANGUAGE NoMonomorphismRestriction #-}
module My.Prelude where

import My.Control.Monad
import Debug.Trace as Tr
import qualified Data.ByteString as B
import Data.Char
import Data.Word
import Data.List

debugMess mess x = Tr.trace (mess++show x) x
debug x = traceShow x x
trace = Tr.trace

maybeToEither = maybe (Left undefined) Right
eitherToMaybe = either (const Nothing) Just

ifThenElse b th el = if b then th else el

swap (a,b) = (b,a)

showHex n = reverse $ map (intToDigit . fromIntegral . (`mod`16)) $ take 2 $ iterate (`div`16) (n :: Word8)
showCode code = intercalate " " $ map showHex $ B.unpack $ code

tagIO tag m = prog1 (putStr (tag++"...") >> m) (putStrLn "done.")
