
data Operator = Plus | Minus | Times | Divide
              deriving (Show, Eq)

type Unit = String
data ParseNode = UnitsNumber Float Unit
               | Operation Operator ParseNode ParseNode
               deriving (Show)

type ArithFunc = Float -> Float -> Float

data Token = TokOp Operator
           | TokNum Float
           | TokUnits String
           | TokOpenParen
           | TokCloseParen
           deriving (Show)

tokenize :: String -> [Token]
tokenize s = map toToken (words s)
  where
    toToken "+" = TokOp Plus
    toToken "*" = TokOp Times
    toToken "/" = TokOp Divide
    toToken "(" = TokOpenParen
    toToken ")" = TokCloseParen
    toToken w
     | all ((flip elem (['a'..'z']++['_']))) w = TokUnits w
     | otherwise = TokNum (read w)

parse :: [Token] -> ParseNode
parse [TokNum f, TokUnits s] = UnitsNumber f s
parse (t1:t2:t3:ts) = case (t1,t2,t3) of
  (TokNum f, TokUnits s, TokOp op) -> Operation op (UnitsNumber f s) (parse ts)
  (_, _, _) -> error "Bad parse"

opFunc :: Operator -> (ParseNode -> ParseNode -> ParseNode)
opFunc op = case op of
                 Plus -> unitsIdentity (+)
                 Minus -> unitsIdentity (-)
                 Times -> times
                 Divide -> divide

unitsIdentity :: ArithFunc -> ParseNode -> ParseNode -> ParseNode
unitsIdentity func (UnitsNumber v1 u1) (UnitsNumber v2 u2)
     | u1 == u2  = (UnitsNumber (func v1 v2) u1)
     | otherwise = error "Dimensional Mismatch"
unitsIdentity func n1 n2 = unitsIdentity func (crunch n1) (crunch n2)

times :: ParseNode -> ParseNode -> ParseNode
times (UnitsNumber v1 u1) (UnitsNumber v2 u2) =
        Operation Times
          (UnitsNumber (v1 * v2) u1)
          (UnitsNumber 1 u2)
times n m = times (crunch n) (crunch m)

divide :: ParseNode -> ParseNode -> ParseNode
divide (UnitsNumber v1 u1) (UnitsNumber v2 u2) =
         Operation Divide
           (UnitsNumber (v1 / v2) u1)
           (UnitsNumber 1 u2)

crunch :: ParseNode -> ParseNode
crunch n@(UnitsNumber f u) = n
crunch   (Operation op node1 node2) = (opFunc op) (crunch node1) (crunch node2)

calculator :: String -> String
calculator s = (show value) ++ " " ++ units
           where (UnitsNumber value units) = crunch $ parse $ tokenize s
                
main = interact calculator