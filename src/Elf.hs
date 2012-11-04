{-# LANGUAGE ForeignFunctionInterface #-}
module Elf (writeElf,entryAddress) where

import Foreign
import Foreign.C
import System.Posix.IO
import System.Posix.Files
import Data.ByteString
import Data.ByteString.Unsafe

foreign import ccall "writeElf" 
  c_writeElf :: CInt -> Ptr CChar -> CInt -> IO ()
foreign import ccall "entryAddress"
  entryAddress :: Int
                
outFileMode = ownerModes + groupReadMode + groupExecuteMode + otherReadMode + otherExecuteMode
  where (+) = unionFileModes
               
writeElf :: String -> ByteString -> IO ()
writeElf name dat = do
  fd <- createFile name outFileMode
  unsafeUseAsCStringLen dat (\(d,nd) -> c_writeElf (fromIntegral fd) d (fromIntegral nd))
                                            


