module DimensionalCalculator.Language.Types where
import DimensionalCalculator.Units
    
data Expression = UnitsNumber Double ComplexUnit
                | Operation BinaryOperator Expression Expression
                deriving (Eq, Show)
                
data BinaryOperator = Plus | Minus | Times | Divide
              deriving (Eq, Show)
