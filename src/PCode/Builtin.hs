-- |A module describing Alpha's primitives
module PCode.Builtin where

-- |An enumeration of all handled Alpha primitives
data Builtin = BAdd | BMul | BSub | BDiv | BMod 
             | BLowerEq | BLowerThan | BGreaterEq | BGreaterThan 
             | BEqual | BNotEqual
             | BAnd | BOr | BXor | BNot
             | BSet | BSetSX
             deriving (Show,Eq,Bounded,Enum)

-- |Is the builtin for a binary operation ?
isBinOp :: Builtin -> Bool
-- |Is it a comparison operation ?
isCompare :: Builtin -> Bool
isBinOp b = b`elem`[BAdd,BSub,BMul,BDiv,BMod,BAnd,BOr,BXor]
isCompare b = b`elem`[BLowerEq,BLowerThan,BGreaterThan,BGreaterEq,BEqual,BNotEqual]

-- |A list of the builtins' names
bNames :: [(Builtin, String)]
bNames = [(BAdd,"+"),(BMul,"*"),(BSub,"-"),(BDiv,"/"),(BMod,"%"),
          (BLowerThan,"<"),(BGreaterThan,">"),(BLowerEq,"<="),(BGreaterEq,">="),
          (BEqual,"=="),(BNotEqual,"<>"),
          (BAnd,"&"),(BOr,"|"),(BXor,"x|"),(BNot,"not"),
          (BSet,"^"),(BSetSX,"¨")]

