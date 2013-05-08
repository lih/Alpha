{-# LANGUAGE NoMonomorphismRestriction #-}
module Misc(debugMess,debug,Tr.trace
                 ,maybeToEither,eitherToMaybe
                 ,ifThenElse
                 ,swap
                 ,($^)
                 ,showHex,showCode
                 ,tagIO,mkIORef,module Data.IORef) where

import My.Control.Monad
import Debug.Trace as Tr
import qualified Data.ByteString as B
import Data.Char
import Data.Word
import Data.List
import Data.IORef
import System.IO.Unsafe

debugMess mess x = Tr.trace (mess++show x) x
debug x = traceShow x x
trace = Tr.trace

maybeToEither = maybe (Left undefined) Right
eitherToMaybe = either (const Nothing) Just

ifThenElse b th el = if b then th else el

swap (a,b) = (b,a)
($^) = flip

showHex n = reverse $ map (intToDigit . fromIntegral . (`mod`16)) $ take 2 $ iterate (`div`16) (n :: Word8)
showCode code = intercalate " " $ map showHex $ B.unpack $ code

tagIO tag m = putStr (tag++"...") >> m <* putStrLn "done."

mkIORef = unsafePerformIO . newIORef

