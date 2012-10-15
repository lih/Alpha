module Syntax where

import Prelude hiding (foldl)
import Data.List (intercalate)
import Data.Foldable
import Data.Traversable
import Control.Applicative

data Syntax a = Group [Syntax a]
              | Symbol a
              deriving Eq
               
instance Functor Syntax where
  fmap f (Symbol s) = Symbol $ f s
  fmap f (Group l) = Group $ map (fmap f) l
instance Foldable Syntax where
  foldl f e (Symbol s) = f e s
  foldl f e (Group l) = foldl (foldl f) e l
instance Traversable Syntax where
  traverse f (Symbol s) = Symbol <$> f s
  traverse f (Group l) = Group <$> traverse (traverse f) l 

instance Show a => Show (Syntax a) where
  show (Symbol s) = show s
  show (Group l) = "[" ++ intercalate " " (map show l) ++ "]"

-- Copyright (c) 2012, Coiffier Marc <marc.coiffier@gmail.com>
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

--     Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--     Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

