module DimensionalCalculator.Language.Types where

data Expression = UnitsNumber Double ComplexUnit
                | Operation BinaryOperator Expression Expression
                deriving (Eq, Show)
                
data BinaryOperator = Plus | Minus | Times | Divide
              deriving (Eq, Show)

-- Units are user-defined, so a string is fitting.
type Unit = String

-- A complex unit is just a big fraction, with some units on top and
-- some on the bottom.
data ComplexUnit = ComplexUnit { numer   :: [Unit]
                               , denom :: [Unit]
                               } deriving (Eq, Show)

class NumberLike a where
  (%+) :: a -> a -> a
  (%-) :: a -> a -> a
  (%*) :: a -> a -> a
  (%/) :: a -> a -> a
  inverse :: a -> a
  
instance NumberLike ComplexUnit where
  (%+) u v = u
  (%-) u v = u
  
  (%*) u v = ComplexUnit {numer=u1++v1, denom=u2++v2}
    where (ComplexUnit {numer=u1, denom=u2}) = u
          (ComplexUnit {numer=v1, denom=v2}) = v
  (%/) u v = u %* (inverse v)
  
  inverse (ComplexUnit {numer=n, denom=d}) = ComplexUnit {numer=d, denom=n}