{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module Format where

import Foreign
import Foreign.C
import Foreign.Marshal.Alloc
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.Monoid
import System.Posix.Files

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
  stat <- getFileStatus name
  let (+) = unionFileModes
  setFileMode name (ownerExecuteMode + groupExecuteMode + otherExecuteMode + fileMode stat)

#define IMPORT_FUN(fun,t) foreign import ccall #fun fun :: t
#define IMPORT_FORMAT(fmt) IMPORT_FUN(fmt##_entry,Int) ; IMPORT_FUN(fmt##_headerSize,Int) ; IMPORT_FUN(fmt##_headerCons,Int -> IO (Ptr CChar)) ; fmt = F #fmt fmt##_entry fmt##_headerSize fmt##_headerCons

raw entry = F ("raw:"++show entry) entry 0 (const $ return nullPtr)
IMPORT_FORMAT(elf64) ; IMPORT_FORMAT(elf32) ; IMPORT_FORMAT(exe)
formats = [elf64,elf32,exe]

#if x86_64_HOST_ARCH
defaultFormat = elf64
#else
defaultFormat = raw 0
#endif



