module DimensionalCalculator.Units where
import qualified Data.Set as S

-- Units are user-defined, so a string is fitting.
type Unit = String

-- A complex unit is just a big fraction, with some units on top and
-- some on the bottom.
type ComplexUnit = ([Unit], [Unit])

errorUnit = "Error"
            
commensurable :: ComplexUnit -> ComplexUnit -> ComplexUnit
commensurable u v
    | u == v    = u
    | otherwise = ([errorUnit], [])

(%+) = commensurable
(%-) = commensurable

(%*) :: ComplexUnit -> ComplexUnit -> ComplexUnit
(%*) (u1,u2) (v1,v2) = (u1 ++ v1, u2 ++ v2)

(%/) :: ComplexUnit -> ComplexUnit -> ComplexUnit
(%/) u v = u %* (inverse v)

inverse :: ComplexUnit -> ComplexUnit
inverse (n,d) = (d,n)