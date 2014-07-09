module Matrix

%access public

Matrix : Nat -> Nat -> Type -> Type
Matrix rows cols a = Vect rows (Vect cols a)


  
dotProduct : (Num a) => Vect (S n) a -> Vect (S n) a -> a
dotProduct a b = foldr1 (+) $ zipWith (*) a b


crossProduct : (Num a) => Vect 3 a -> Vect 3 a -> Vect 3 a
crossProduct [a1, a2, a3] [b1, b2, b3] = [a2*b3 - a3*b2, a3*b1 - a1*b3, a1*b2 - b1*a2]


crossProduct2 : (Num a) => Vect 2 a -> Vect 2 a -> a
crossProduct2 a b = head $ crossProduct (0::a) (0::b)


-- multiplies a matrix by a vector
multVect : (Num a) => Matrix (S m) (S n) a -> Vect (S n) a -> Vect (S m) a
multVect vs v = map (dotProduct v) vs


-- matrix multiplication
multMatrix : (Num a) => Matrix (S n) (S o) a -> Matrix (S m) (S n) a -> Matrix (S m) (S o) a
multMatrix a (bCols) = map (multVect (transpose a)) bCols


identityMatrix : (Num a) => Matrix n n a
identityMatrix {n = Z}   = []
identityMatrix {n = S m} = (one :: replicate m zero) :: (map (zero ::) identityMatrix) where
  zero = fromInteger 0
  one = fromInteger 1
