module Context.Types where

import Foreign.ForeignPtr
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
  maxID    :: ID,
  symMap   :: Bimap String ID,
  aliasMap :: Map ID ID,
  equivMap :: Map ID ID,
  modMap   :: Bimap String IDRange,
  valMap   :: Map ID Context.Types.Value,
  exports  :: Set ID,
  loadCode :: [Instruction]
  }
symsF = Field (symMap,\s ce -> ce { symMap = s })
valsF = Field (valMap,\v ce -> ce { valMap = v })

data Context = C {
  language      :: Language,
  jitAddresses  :: Map ID (ForeignPtr Word8),
  compAddresses :: Map ID (Int,ForeignPtr Word8),
  compTop       :: Int,
  transform     :: Syntax ID -> IO (Syntax ID)
  }
jitAddressesF  = Field (jitAddresses,\a c -> c { jitAddresses = a })
compAddressesF = Field (compAddresses,\a c -> c { compAddresses = a })
languageF      = Field (language,\a c -> c { language = a })
compTopF       = Field (compTop,\a c -> c { compTop = a })


