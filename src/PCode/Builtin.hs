module PCode.Builtin where

data Builtin = BAdd | BMul | BSub | BDiv | BMod 
             | BAnd | BOr | BXor | BNot
             | BLowerEq | BLowerThan | BGreaterEq | BGreaterThan 
             | BEqual | BNotEqual
             | BCall
             | BSet
             deriving (Show,Eq)

bNames = [(BAdd,"+"),(BMul,"*"),(BSub,"-"),(BDiv,"/"),(BMod,"%"),
          (BLowerThan,"<"),(BGreaterThan,">"),(BLowerEq,"<="),(BGreaterEq,">="),
          (BEqual,"=="),(BNotEqual,"<>"),
          (BAnd,"&"),(BOr,"|"),(BXor,"x|"),(BNot,"not")]


