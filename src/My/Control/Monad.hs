{-# LANGUAGE NoMonomorphismRestriction #-}
module My.Control.Monad (module Control.Monad,($<),(>$),(>$<),(§),(§<),(>§),(>§<),(<&&>),(<||>),ifM,findM,traverseM) where

import Control.Monad
import Data.Traversable
import Control.Applicative

($<)  :: Monad m => (a -> b) -> m a -> m b
(>$)  :: Monad m => m (a -> b) -> a -> m b
(>$<) :: Monad m => m (a -> b) -> m a -> m b
($<) = liftM
mf >$ x = mf >>= \f -> return (f x)
(>$<) = ap
          
(§) = flip ($)
(§<) = flip (>$)
(>§) = flip ($<)
(>§<) = flip (>$<)      

ifM b th el = b >>= \b -> if b then th else el
a <&&> b = ifM a b (return False)
a <||> b = ifM a (return True) b

findM p l = foldr fun (return Nothing) l
  where fun x ret = ifM (p x) (return $ Just x) ret

traverseM f = unwrapMonad . traverse (WrapMonad . f)

-- Copyright (c) 2012, Coiffier Marc <marc.coiffier@gmail.com>
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

--     Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--     Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DPCodeECT, INDPCodeECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

