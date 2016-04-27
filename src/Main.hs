module Main where
import DimensionalCalculator.Language.Types
import DimensionalCalculator.Language.Parser (parseExpr)

type ArithFunc = Double -> Double -> Double

opFunc :: BinaryOperator -> ArithFunc
opFunc op = case op of
                 Plus -> (+)
                 Minus -> (-)
                 Times -> (*)
                 Divide -> (/)

crunchValue :: Expression -> Double
crunchValue n@(UnitsNumber v u) = v
crunchValue (Operation op e1 e2) = (opFunc op) (crunchValue e1) (crunchValue e2)

crunchUnits :: Expression -> ComplexUnit
crunchUnits (UnitsNumber _ u) = u
crunchUnits (Operation op e1 e2)
    | op == Plus && sameUnits = u1 %+ u2
    | op == Minus && sameUnits = u1 %- u2
    | op == Times = u1 %* u2
    | op == Divide = u1 %/ u2
    where
      u1 = crunchUnits e1
      u2 = crunchUnits e2
      sameUnits = u1 == u2
                  
crunch :: Expression -> Expression
crunch e = UnitsNumber (crunchValue e) (crunchUnits e)

calculator :: String -> String
calculator s = (show value) ++ " " ++ (show units)
           where
             expr = parseExpr s
             value = crunchValue expr
             units = crunchUnits expr
                
main = interact calculator