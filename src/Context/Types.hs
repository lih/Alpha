module Context.Types(
  -- * Global Values
  Value(..),references,
  -- ** The Axiom enumeration
  Axiom(..),
  -- * The Language type
  Language(..),
  -- * The Context type
  Context(..),
  -- * Views
  syms_,vals_,jitAddresses_,compAddresses_,language_,compTop_
  ) where

import Foreign.ForeignPtr
import Foreign.Ptr
import Data.Word
import Data.Bimap
import Data.Map
import Data.Set
import Data.View
import ID
import PCode hiding (Value(..))

data Axiom = XAlter | XBind
           | XReturn | XRestart | XChoose | XDo
           | XAddr | XSize
           | XLang | XVerb | XNoun | XID
           deriving Show

-- |The global Value type
data Value = Axiom Axiom
           | Builtin Builtin
           | Verb Code
           | Noun Code Code
           | NoValue
           deriving Show

-- |Returns a list of symbols referred to by the given Value
references (Verb code) = codeRefs code 
references (Noun sz i) = codeRefs sz ++ codeRefs i
references _ = []

{-|
Languages are what passes for modules in the Alpha world. They gather
all the information that is needed to compile, specialize and link
programs distributed across several files and/or executions.

Integral to the notion of language is that of symbols. Symbols are
unique identifiers that are sometimes associated to names (but may also
be anonymous) and that describe 
-}
data Language = Language {
  nameL       :: String,
  maxIDL      :: ID,
  symbolsL    :: Bimap String ID,
  aliasesL    :: Map ID ID,
  equivsL     :: Map ID ID,
  languagesL  :: Bimap String (Range ID),
  valuesL     :: Map ID Value,
  exportsL    :: Set ID
  }
syms_ = View symbolsL (\s ce -> ce { symbolsL = s })
vals_ = View valuesL  (\v ce -> ce { valuesL = v })

data Context = C {
  language      :: Language,
  jitAddresses  :: Map ID (ForeignPtr Word8),
  compAddresses :: Map ID (Int,ForeignPtr Word8),
  compTop       :: Int,
  transform     :: Maybe (Ptr ())
  }
jitAddresses_  = View jitAddresses  (\a c -> c { jitAddresses = a })
compAddresses_ = View compAddresses (\a c -> c { compAddresses = a })
language_      = View language      (\a c -> c { language = a })
compTop_       = View compTop       (\a c -> c { compTop = a })
