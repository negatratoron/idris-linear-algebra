module Matrix

import MatrixBase
import Vect

%access public

(*) : (Num a) => Matrix (S n) (S o) a -> Matrix (S m) (S n) a -> Matrix (S m) (S o) a
(*) a (bCols) = map ((transpose a) *) bCols


identityMatrix : (Num a) => Matrix n n a
identityMatrix {n = Z}   = []
identityMatrix {n = S m} = (one :: replicate m zero) :: (map (zero ::) identityMatrix) where
  zero = fromInteger 0
  one = fromInteger 1
