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
import Options

data Axiom = XAlter | XBind
           | XReturn | XRestart | XChoose | XDo
           | XAddr | XSize
           | XLang | XVerb | XNoun | XID
           deriving Show

data Value = Axiom Axiom
           | Builtin Builtin
           | Verb Code
           | Noun Code Code
           | NoValue
           deriving Show

references (Verb code) = codeRefs code 
references (Noun size init) = codeRefs size ++ codeRefs init
references _ = []

data Language = Language {
  nameL       :: String,
  maxIDL      :: ID,
  symbolsL    :: Bimap String ID,
  aliasesL    :: Map ID ID,
  equivsL     :: Map ID ID,
  languagesL  :: Bimap String (Range ID),
  valuesL     :: Map ID Context.Types.Value,
  exportsL    :: Set ID
  }
syms_ = View (symbolsL,\s ce -> ce { symbolsL = s })
vals_ = View (valuesL,\v ce -> ce { valuesL = v })

data Context = C {
  settings      :: Settings,
  language      :: Language,
  jitAddresses  :: Map ID (ForeignPtr Word8),
  compAddresses :: Map ID (Int,ForeignPtr Word8),
  compTop       :: Int,
  transform     :: Maybe (Ptr ())
  }
jitAddresses_  = View (jitAddresses,\a c -> c { jitAddresses = a })
compAddresses_ = View (compAddresses,\a c -> c { compAddresses = a })
language_      = View (language,\a c -> c { language = a })
compTop_       = View (compTop,\a c -> c { compTop = a })
