module Syntax.Parse (parseAlpha) where

import Syntax
import Text.ParserCombinators.Parsec hiding (spaces)
import My.Control.Monad
import Data.Maybe

parseAlpha file s = lazyMany axiom file s

-- Utilities
lazyMany p file contents = lm state0
      where
        Right state0 = parse getParserState file contents

        lm state = case parse p' "" "" of
          Left err -> error (show err)
          Right x -> x
          where
            p' = do
              setParserState state
              choice [
                try (oWhite >> eof >> return []),
                liftM2 (:) (p >>= \p -> optional (oneOf ",;") >> return p) $ lm $< getParserState]
wrParen e@(_:_:_) = mkNode e
wrParen e = concat e
mkNode = return . Group . concat
free :: Parser a -> Parser a
free e = try $ between oWhite oWhite e

-- Productions
axiom = free boundExpr

atom = liftM return symbol <|> parGroup

wrap beg g end = between (string beg) (string end) g
parGroup = wrap "(" (inParen wrParen) ")"
        <|> wrap "[" (inParen mkNode) "]"
        <|> wrap "{" (inParen concat) "}"
inParen wr = wr $< oWhite >> looseExpr`endBy`oWhite

chain expr op e = (do o <- try op
                      e' <- expr
                      chain expr op (o e e'))
                  <|> return e

looseExpr = (<?>"loose expression") $ concat $< (boundExpr `sepBy1` free (oneOf ",;"))
boundExpr = (<?>"bound expression") $ wrParen $< (infExpr `sepBy1` free (char '_'))
infExpr = (<?>"infix expression") $ chain (return ()) op =<< tightExpr
  where opExpr = (<?>"operator expression") $ concat $< many1 atom
        op = (free (char '.') >> liftM (\o e _ -> [Group $ o++e]) opExpr)
             <|> (free (char ':') >> liftM2 (\o e' e _ -> [Group $ o++e++e']) opExpr (oWhite >> tightExpr))
tightExpr = (<?>"close expression") $ wrParen $< many1 atom

symbol =  Symbol $< ((string <?> "string") <|> (symbol <?> "symbol"))
  where string = do
          sep <- (char '\'' >> anyChar) <|> char '"'
          str <- many (quoted $ satisfy (/=sep))
          char sep
          return $ '\'':sep:str
        symbol = many1 $ quoted $ noneOf " \t\n()[]{},;.:_"
        quoted p = (char '\\' >> (esc $< anyChar)) <|> p
        esc c = fromMaybe c $ lookup c [('n','\n'),('t','\t')]

white = skipMany1 ((comment <|> spaces) <?> "whitespace")
  where spaces = skipMany1 $ space
        comment = (char '~' >> optional spaces >> void (many1 atom)) <?> "comment"
oWhite = optional $ white

-- Copyright (c) 2012, Coiffier Marc <marc.coiffier@gmail.com>
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

--     Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--     Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
