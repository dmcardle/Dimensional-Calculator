import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T
import qualified Text.ParserCombinators.Parsec.Language as L

data Operator = Plus | Minus | Times | Divide
              deriving (Eq, Ord, Show)

type Unit = String
data Expression = UnitsNumber Double Unit
                | Operation Operator Expression Expression
                deriving (Eq, Ord, Show)

type ArithFunc = Float -> Float -> Float

data Token = TokOp Operator
           | TokNum Float
           | TokUnits String
           | TokOpenParen
           | TokCloseParen
           deriving (Eq, Ord, Show)

expr :: Parser Expression
expr = try $ unitsNum <|> operation

unitsNum :: Parser Expression
unitsNum = try $ do
  n <- T.float lexer
  u <- T.identifier lexer
--  notFollowedBy $ lookAhead operator
  return (UnitsNumber n u)

operation :: Parser Expression
operation = try $ do
  e1 <- expr
  op <- operator
  e2 <- expr
  return (Operation op e1 e2)
  
operator :: Parser Operator
operator = do
  op <- T.operator lexer
  let ret =
        case op of
          "+" -> Plus
          "-" -> Minus
          "*" -> Times
          "/" -> Divide
    in
   return ret
  
-- Borrowing Haskell lexer for the calculator's expression language
lexer = L.haskell

parseExpr :: String -> Expression
parseExpr str =
  case parse expr "" str of
    Left e  -> error $ show e
    Right r -> r

parseOper :: String -> Expression
parseOper str =
  case parse operation "" str of
    Left e  -> error $ show e
    Right r -> r

parseUnitsNum :: String -> Expression
parseUnitsNum str =
  case parse unitsNum "" str of
    Left e  -> error $ show e
    Right r -> r
