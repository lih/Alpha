module Syntax.Parse (parseAlpha) where

import Syntax
import Text.ParserCombinators.Parsec hiding (spaces)
import My.Control.Monad hiding ((<|>),optional,many)
import Data.Maybe

parseAlpha file s = concat $ lazyMany axiom file s

-- Utilities
lazyMany p file contents = lm state0
  where Right state0 = parse getParserState file contents
        lm state = case parse p' "" "" of
          Left err -> error (show err)
          Right x -> x
          where p' = setParserState state >>
                     (try (oWhite *> eof *> pure [])
                      <|> liftA2 (:) (p <* optional (oneOf ",;")) (lm <$> getParserState))
wrParen e@(_:_:_) = mkNode e
wrParen e = concat e
mkNode = return . Group . concat
free :: Parser a -> Parser a
free e = try $ between oWhite oWhite e

-- Productions
axiom = free boundExpr

atom = (return <$> symbol) <|> parGroup

wrap beg g end = between (string beg) (string end) g
parGroup = wrap "(" (inParen wrParen) ")"
        <|> wrap "[" (inParen mkNode) "]"
        <|> wrap "{" (inParen concat) "}"
inParen wr = wr <$> (oWhite >> looseExpr`endBy`oWhite)

chain expr op e = (do o <- try op
                      e' <- expr
                      chain expr op (o e e'))
                  <|> return e

looseExpr = (<?>"loose expression") $ concat <$> (boundExpr `sepBy1` free (oneOf ",;"))
boundExpr = (<?>"bound expression") $ wrParen <$> (infExpr `sepBy1` free (char '_'))
infExpr = (<?>"infix expression") $ chain (return ()) op =<< tightExpr
  where opExpr = (<?>"operator expression") $ concat <$> many1 atom
        op = (free (char '.') >> liftM (\o e _ -> [Group $ o++e]) opExpr)
             <|> (free (char ':') >> liftM2 (\o e' e _ -> [Group $ o++e++e']) opExpr (oWhite >> tightExpr))
tightExpr = (<?>"close expression") $ wrParen <$> many1 atom

symbol =  Symbol <$> ((_string <?> "string") <|> (_symbol <?> "symbol"))
  where _string = do
          sep <- (char '\'' >> anyChar) <|> char '"'
          str <- many (quoted $ satisfy (/=sep))
          char sep
          return $ '\'':sep:str
        _symbol = many1 $ quoted $ noneOf " \t\n()[]{},;.:_"
        quoted p = (char '\\' >> (esc <$> anyChar)) <|> p
        esc c = fromMaybe c $ lookup c [('n','\n'),('t','\t')]

white = skipMany1 ((comment <|> spaces) <?> "whitespace")
  where spaces = skipMany1 $ space
        comment = (char '~' >> optional spaces >> void (many1 atom)) <?> "comment"
oWhite = optional white

