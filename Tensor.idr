module Tensor


-- this data type provides more machinery than most of this library actually needs
-- it allows for n-dimensional arrays
-- this library mostly deals with plain old vectors and matrices
data Tensor : (dim : Nat) -> Vect dim Nat -> Type -> Type where
  Scalar : a -> Tensor 0 Nil a
  Vector : Vect d (Tensor dim ds a) -> Tensor (S dim) (d::ds) a

-- column vector
Vec : Nat -> Type -> Type
Vec m = Tensor 1 [m]

-- Matrix row-length column-length
Matrix : Nat -> Nat -> Type -> Type
Matrix m n = Tensor 2 [m, n]

-- consT pushes a tensor onto the front of another tensor
-- the tensors' dimensions must differ by 1
-- (too bad this method can't be used to pattern match on tensors)
consT : (Tensor dim ds a) ->
        (Tensor (S dim) (d::ds) a) ->
        (Tensor (S dim) ((S d)::ds) a)
consT t (Vector ts) = Vector (t::ts)


instance Functor (Tensor dim ds) where
  map f (Scalar a) = Scalar (f a)
  map f (Vector v) = Vector (map (map f) v)


instance (Eq a) => Eq (Tensor dim ds a) where
  (Scalar a) == (Scalar b) = a == b
  (Vector a) == (Vector b) = a == b

instance Foldable (Tensor dim ds) where
  foldr f e (Scalar a) = f a e
  foldr f e (Vector v) = foldr (flip (foldr f)) e v

instance (Show a) => Show (Tensor dim ds a) where
  show (Scalar a) = show a
  show (Vector v) = show v


transpose : Matrix a b t -> Matrix b a t
transpose {b} (Vector Nil) = Vector $ replicate b (Vector Nil)
transpose (Vector ((Vector v)::vs)) = let (Vector os) = transpose (Vector vs) in
  Vector (zipWith consT v os)


(+) : (Num a) => Tensor dim ds a -> Tensor dim ds a -> Tensor dim ds a
(+) (Scalar a) (Scalar b) = Scalar (a + b)
(+) (Vector a) (Vector b) = Vector (zipWith Tensor.(+) a b)


instance (Num a) => Num (Tensor Z Nil a) where
  (+) = Tensor.(+)
  (Scalar a) * (Scalar b) = Scalar (a * b)
  (Scalar a) - (Scalar b) = Scalar (a - b)
  abs (Scalar a) = Scalar (abs a)
  fromInteger = Scalar . fromInteger
  

zipWith : (a -> b -> c) -> Tensor dim ds a -> Tensor dim ds b -> Tensor dim ds c
zipWith f (Scalar a) (Scalar b) = Scalar (f a b)
zipWith f (Vector a) (Vector b) = Vector $ zipWith (zipWith f) a b


dotProduct : (Num a) => Vec (S n) a -> Vec (S n) a -> Tensor Z Nil a
dotProduct a b = let (Vector vs) = zipWith (*) a b in
  foldr1 Tensor.(+) vs


-- would be cool to implement this in terms of wedge product
-- instead of brute forcing it term by term
crossProduct : (Num a) => Vec 3 a -> Vec 3 a -> Vec 3 a
crossProduct (Vector [a1, a2, a3]) (Vector [b1, b2, b3]) =
  Vector ([a2*b3 - a3*b2, a3*b1 - a1*b3, a1*b2 - b1*a2])


crossProduct2 : (Num a) => Vec 2 a -> Vec 2 a -> Tensor Z Nil a
crossProduct2 a b = let Vector [x, y, z] = crossProduct (consT 0 a) (consT 0 b) in z


--multVector : Matrix m n -> Vec m -> Vec n


--multMatrix : Matrix n o -> Matrix m n -> Matrix m o
--multMatrix 


SquareMatrix : Nat -> Type -> Type
SquareMatrix m = Tensor 2 [m, m]


