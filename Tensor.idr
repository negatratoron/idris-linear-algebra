module Tensor


-- this data type provides more machinery than most of this library actually needs
-- it allows for n-dimensional arrays
-- this library mostly deals with plain old vectors and matrices
-- a danger is that this machinery turns out poor for higher-dimensional use cases

data Tensor : (dim : Nat) -> Vect dim Nat -> Type -> Type where
  MkScalar : a -> Tensor 0 Nil a
  MkVector : Vect d (Tensor dim ds a) -> Tensor (S dim) (d::ds) a


--------------------------------------------------------------------------------
-- More Tensor type constructors
--------------------------------------------------------------------------------

Scalar : Type -> Type
Scalar = Tensor Z Nil

Vec : Nat -> Type -> Type
Vec rows = Tensor 1 [rows]

Matrix : Nat -> Nat -> Type -> Type
Matrix rows cols = Tensor 2 [rows, cols]

SquareMatrix : Nat -> Type -> Type
SquareMatrix m = Tensor 2 [m, m]


--------------------------------------------------------------------------------
-- More Tensor constructors
--------------------------------------------------------------------------------

consT : (Tensor dim ds a) ->
        (Tensor (S dim) (d::ds) a) ->
        (Tensor (S dim) ((S d)::ds) a)
consT t (MkVector ts) = MkVector (t::ts)


--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Functor (Tensor dim ds) where
  map f (MkScalar a) = MkScalar (f a)
  map f (MkVector v) = MkVector (map (map f) v)

instance (Eq a) => Eq (Tensor dim ds a) where
  (MkScalar a) == (MkScalar b) = a == b
  (MkVector a) == (MkVector b) = a == b

instance Foldable (Tensor dim ds) where
  foldr f e (MkScalar a) = f a e
  foldr f e (MkVector v) = foldr (flip (foldr f)) e v

instance (Show a) => Show (Tensor dim ds a) where
  show (MkScalar a) = show a
  show (MkVector v) = show v


-- currently Num only applies to Scalars
-- not sure if multiplication fits
(+) : (Num a) => Tensor dim ds a -> Tensor dim ds a -> Tensor dim ds a
(+) (MkScalar a) (MkScalar b) = MkScalar (a + b)
(+) (MkVector a) (MkVector b) = MkVector (zipWith Tensor.(+) a b)

instance (Num a) => Num (Tensor Z Nil a) where
  (+) = Tensor.(+)
  (MkScalar a) * (MkScalar b) = MkScalar (a * b)
  (MkScalar a) - (MkScalar b) = MkScalar (a - b)
  abs (MkScalar a) = MkScalar (abs a)
  fromInteger = MkScalar . fromInteger


--------------------------------------------------------------------------------
-- Zips, Folds, Flips, and Flops
--------------------------------------------------------------------------------

zipWith : (a -> b -> c) -> Tensor dim ds a -> Tensor dim ds b -> Tensor dim ds c
zipWith f (MkScalar a) (MkScalar b) = MkScalar (f a b)
zipWith f (MkVector a) (MkVector b) = MkVector $ zipWith (zipWith f) a b


--------------------------------------------------------------------------------
-- Mathematical operations
--------------------------------------------------------------------------------
  
dotProduct : (Num a) => Vec (S n) a -> Vec (S n) a -> Tensor Z Nil a
dotProduct a b = let (MkVector vs) = zipWith (*) a b in
  foldr1 Tensor.(+) vs


-- it would be cool to implement cross product in terms of wedge product
-- instead of brute forcing it term by term
crossProduct : (Num a) => Vec 3 a -> Vec 3 a -> Vec 3 a
crossProduct (MkVector [a1, a2, a3]) (MkVector [b1, b2, b3]) =
  MkVector ([a2*b3 - a3*b2, a3*b1 - a1*b3, a1*b2 - b1*a2])


crossProduct2 : (Num a) => Vec 2 a -> Vec 2 a -> Tensor Z Nil a
crossProduct2 a b = let MkVector [x, y, z] = crossProduct (consT 0 a) (consT 0 b) in z


transpose : Matrix a b t -> Matrix b a t
transpose {b} (MkVector Nil) = MkVector $ replicate b (MkVector Nil)
transpose (MkVector ((MkVector v)::vs)) = let (MkVector os) = transpose (MkVector vs) in
  MkVector (zipWith consT v os)


-- multiplies a matrix by a vector
multVec : (Num a) => Matrix (S m) (S n) a -> Vec (S n) a -> Vec (S m) a
multVec (MkVector vs) v = MkVector (map (dotProduct v) vs)


-- matrix multiplication
multMatrix : (Num a) => Matrix (S n) (S o) a -> Matrix (S m) (S n) a -> Matrix (S m) (S o) a
multMatrix a (MkVector bCols) = MkVector $ map (multVec (transpose a)) bCols

