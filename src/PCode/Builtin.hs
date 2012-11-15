module PCode.Builtin where

data Builtin = BAdd | BMul | BSub | BDiv | BMod 
             | BAnd | BOr | BXor | BNot
             | BLowerEq | BLowerThan | BGreaterEq | BGreaterThan 
             | BEqual | BNotEqual
             | BCall
             | BSet | BSetSX
             deriving (Show,Eq)

isBinOp b = b/=BCall && b/=BSet && b/=BNot

bNames = [(BAdd,"+"),(BMul,"*"),(BSub,"-"),(BDiv,"/"),(BMod,"%"),
          (BLowerThan,"<"),(BGreaterThan,">"),(BLowerEq,"<="),(BGreaterEq,">="),
          (BEqual,"=="),(BNotEqual,"<>"),
          (BAnd,"&"),(BOr,"|"),(BXor,"x|"),(BNot,"not"),
          (BSet,"^"),(BSetSX,"¨")]

