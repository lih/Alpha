module PCode.Value where

import Control.Monad
import Control.Monad.Instances
import Data.Char
import Data.List
import Data.Maybe
import My.Prelude
import ID

data ValType = Value | GValue | Address | Size | SymID
             deriving Eq
data Value = SymVal ValType ID
           | IntVal Integer
           | NullVal

varEqVal n (SymVal Value n') = n==n'
varEqVal _ _ = False
symValType (SymVal t _) = t

readConstant s = let (a,b) = break (=='#') (filter (/='-') s) in eitherToMaybe $
  parseInt 10 a >>= \r -> if null b then return r else parseInt r (tail b)
  where 
    parseInt r s 
      | all isHexDigit s =
        case find (>=r) digs of 
          Just d -> Left $ "digit '"++[intToDigit $ fromIntegral d]++"' too great for base "++show r++" (in numeral constant '"++s++"')" 
          Nothing -> Right $ sum $ zipWith (*) (reverse digs) $ iterate (*r) 1
      | otherwise = Left $ "all digits must be alphanumeric characters (in constant '"++s++"')"
      where digs = map (fromIntegral . digitToInt) s
        
instance Show Value where
  show (SymVal t v)   = prefix++show v
    where prefix = fromJust $ lookup t [(Value,""),(Address,"@"),(Size,"#"),(SymID,"$"),(GValue,"g")]
  show (IntVal n)   = show n
  show NullVal      = "(null)"

