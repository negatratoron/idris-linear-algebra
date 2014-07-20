module LinAlg.Matrix

import Vect

%access public
%default total


||| A Matrix is a Vect of Vects
Matrix : Nat -> Nat -> Type -> Type
Matrix cols rows a = Vect rows (Vect cols a)

||| Multiply a Matrix by a Vector
multVect : (Num a) => Matrix (S m) (S n) a -> Vect (S m) a -> Vect (S n) a
multVect vs v = map (dotProduct v) vs

||| Multiply a Matrix by a Matrix
multMat : (Num a) => Matrix (S m) (S n) a -> Matrix (S n) (S o) a -> Matrix (S m) (S o) a
multMat a b = map (multVect (transpose a)) b

||| Identity matrix
identityMatrix : (Num a) => Matrix n n a
identityMatrix {n = Z}   = []
identityMatrix {n = S m} = (one :: replicate m zero) :: (map (zero ::) identityMatrix) where
  zero = fromInteger 0
  one  = fromInteger 1

