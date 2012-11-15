module Context.Types where

import Foreign.ForeignPtr
import Foreign.Ptr
import Data.Word
import Data.Bimap
import Data.Map
import Data.Set
import My.Control.Monad.State
import ID
import PCode
import Syntax

data Axiom = XAlter | XBind
           | XReturn | XRestart | XChoose | XDo
           | XLang | XVerb | XNoun
           | XID | XAddr | XSize
           deriving Show

data Value = Axiom Axiom
           | Builtin Builtin
           | Verb Code
           | Noun Code Code
           | NoValue
           deriving Show

data Language = Language {
  maxIDL      :: ID,
  symbolsL    :: Bimap String ID,
  aliasesL    :: Map ID ID,
  equivsL     :: Map ID ID,
  languagesL  :: Bimap String (Range ID),
  valuesL     :: Map ID Context.Types.Value,
  exportsL    :: Set ID,
  initializeL :: [Instruction]
  }
symsF = Field (symbolsL,\s ce -> ce { symbolsL = s })
valsF = Field (valuesL,\v ce -> ce { valuesL = v })

data Context = C {
  language      :: Language,
  jitAddresses  :: Map ID (ForeignPtr Word8),
  compAddresses :: Map ID (Int,ForeignPtr Word8),
  compTop       :: Int,
  transform     :: Maybe (Ptr ())
  }
jitAddressesF  = Field (jitAddresses,\a c -> c { jitAddresses = a })
compAddressesF = Field (compAddresses,\a c -> c { compAddresses = a })
languageF      = Field (language,\a c -> c { language = a })
compTopF       = Field (compTop,\a c -> c { compTop = a })
