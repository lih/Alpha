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
                                            

-- Copyright (c) 2012, Coiffier Marc <marc.coiffier@gmail.com>
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

--     Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--     Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DPCodeECT, INDPCodeECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

