module MatrixBase

import Vect

%access public

Matrix : Nat -> Nat -> Type -> Type
Matrix rows cols a = Vect rows (Vect cols a)

(*) : (Num a) => Matrix (S m) (S n) a -> Vect (S n) a -> Vect (S m) a
(*) vs v = map (dotProduct v) vs


