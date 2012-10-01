module PCode.Value where

import Control.Monad
import Control.Monad.Instances
import Data.Char
import Data.List
import Data.Maybe
import My.Prelude
import ID

data ValType = Value | Address | Size | SymID
             deriving Eq
data Value = SymVal ValType ID
           | IntVal Int
           | NullVal

varEqVal n (SymVal Value n') = n==n'
varEqVal _ _ = False

readConstant s = let (a,b) = break (=='#') (filter (/='-') s) in eitherToMaybe $
  parseInt 10 a >>= \r -> if null b then return r else parseInt r (tail b)

parseInt r s 
  | all isHexDigit s =
    case find (>=r) digs of 
      Just d -> Left $ "digit '"++[intToDigit d]++"' too great for base "++show r++" (in numeral constant '"++s++"')" 
      Nothing -> Right $ sum $ zipWith (*) (reverse digs) $ iterate (*r) 1
  | otherwise = Left $ "all digits must be alphanumeric characters (in constant '"++s++"')"
  where digs = map digitToInt s
        
instance Show Value where
  show (SymVal t v)   = prefix++show v
    where prefix = fromJust $ lookup t [(Value,""),(Address,"@"),(Size,"#"),(SymID,"$")]
  show (IntVal n)   = show n
  show NullVal      = "(null)"


-- Copyright (c) 2012, Coiffier Marc <marc.coiffier@gmail.com>
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

--     Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--     Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DPCodeECT, INDPCodeECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

