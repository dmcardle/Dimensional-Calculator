module DimensionalCalculator.Language.Parser (parseExpr) where
import DimensionalCalculator.Language.Types

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

def = emptyDef{ commentStart = "{-"
              , commentEnd = "-}"
              , identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf "+-*/"
              , opLetter = oneOf "+-*/"
              , reservedOpNames = ["+", "-", "*", "/"]
              , reservedNames = []
              }
      
lexer = makeTokenParser def

TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , semiSep1 = m_semiSep1
           , whiteSpace = m_whiteSpace } = lexer

exprParser :: Parser Expression
exprParser = buildExpressionParser table term <?> "expression"

table = [ [Infix (m_reservedOp "+" >> return (Operation Plus)) AssocLeft]
        , [Infix (m_reservedOp "-" >> return (Operation Minus)) AssocLeft]
        , [Infix (m_reservedOp "*" >> return (Operation Times)) AssocLeft]
        , [Infix (m_reservedOp "/" >> return (Operation Divide)) AssocLeft]
        ]
term = m_parens exprParser
       <|> unitsNumber
       <|> operation

unitsNumber = do
  f <- float lexer
  u <- m_identifier
  return (UnitsNumber f (ComplexUnit {numer=[u], denom=[]}))

operation :: Parser Expression
operation = do
  e1 <- exprParser
  op <- parseOperator
  e2 <- exprParser
  
  return (Operation op e1 e2)
  
parseOperator = (m_reservedOp "+" >> return Plus)
                <|> (m_reservedOp "-" >> return Minus)
                <|> (m_reservedOp "*" >> return Times)
                <|> (m_reservedOp "/" >> return Divide)
                
tokenOf :: String -> BinaryOperator
tokenOf op = case op of
  "+" -> Plus
  "-" -> Minus
  "*" -> Times
  "/" -> Divide

parseExpr :: String -> Expression
parseExpr str =
  case parse exprParser "" str of
    Left e  -> error $ show e
    Right r -> r
