{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module Format where

import Foreign
import Foreign.C
import Foreign.Marshal.Alloc
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.Monoid

data Format = F {
  formatName       :: String,
  formatEntry      :: Int,
  formatHeaderSize :: Int,
  formatHeaderCons :: Int -> IO (Ptr CChar)
  }

instance Show Format where
  show = formatName

writeFormat fmt name dat = do
  hp <- formatHeaderCons fmt (B.length dat)
  h <- BU.unsafePackCStringLen (hp,formatHeaderSize fmt)
  B.writeFile name (h <> dat)

foreign import ccall "elf64_entry"      elf64_entry      :: Int
foreign import ccall "elf64_headerSize" elf64_headerSize :: Int
foreign import ccall "elf64_headerCons" elf64_headerCons :: Int -> IO (Ptr CChar)
foreign import ccall "pe_entry"      pe_entry      :: Int
foreign import ccall "pe_headerSize" pe_headerSize :: Int
foreign import ccall "pe_headerCons" pe_headerCons :: Int -> IO (Ptr CChar)

#if x86_64_HOST_ARCH
defaultFormat = elf64
#else
defaultFormat = raw 0
#endif

elf64 = F "elf64" elf64_entry elf64_headerSize elf64_headerCons
pe = F "pe" pe_entry pe_headerSize pe_headerCons
raw entry = F ("raw("++show entry++")") entry 0 (const $ return nullPtr)


