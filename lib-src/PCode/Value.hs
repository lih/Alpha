module PCode.Value where

import Data.Char
import Data.List
import Data.Maybe
import Misc (eitherToMaybe)
import ID

data ValType = Value | GValue | Address | Size | SymID
             deriving Eq
data Value = SymVal ValType ID
           | IntVal Integer
           | NullVal

varEqVal n (SymVal Value n') = n==n'
varEqVal _ _ = False
symValType (SymVal t _) = t
symValType _ = error $ "symValType should be called with an argument of the form (SymVal t v)"

readConstant s = let (a,b) = break (=='#') (filter (/='-') s) in eitherToMaybe $
  parseInt 10 a >>= \r -> if null b then return r else parseInt r (tail b)
parseInt r s | all isHexDigit s = case find (>=r) digs of 
  Just d -> Left $ "digit '"++[intToDigit $ fromIntegral d]++"' too great for base "++show r++" (in numeral constant '"++s++"')" 
  Nothing -> Right $ sum $ zipWith (*) (reverse digs) $ iterate (*r) 1
              | otherwise = Left $ "all digits must be alphanumeric characters (in constant '"++s++"')"
  where digs = map (fromIntegral . digitToInt) s
        
instance Show Value where
  show (SymVal t (ID v)) = prefix++show v
    where prefix = fromJust $ lookup t [(Value,"x"),(Address,"@"),(Size,"$"),(SymID,"#"),(GValue,"X")]
  show (IntVal n)   = show n
  show NullVal      = "(null)"
