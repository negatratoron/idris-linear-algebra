module LinAlg.Vect

import Data.Floats

%access public
%default total

||| Dot product of two vectors
dotProduct : (Num a) => Vect (S n) a -> Vect (S n) a -> a
dotProduct a b = foldr1 (+) $ zipWith (*) a b

||| Cross product of two 3-vectors
crossProduct : (Num a) => Vect 3 a -> Vect 3 a -> Vect 3 a
crossProduct [a1, a2, a3] [b1, b2, b3] =
  [a2 * b3 - a3 * b2
  ,a3 * b1 - a1 * b3
  ,a1 * b2 - b1 * a2]

||| The nonzero component of the "cross product" of two 2-vectors
||| It makes no difference whether we take x or z to be the nonzero component
crossProduct2 : (Num a) => Vect 2 a -> Vect 2 a -> a
crossProduct2 a b = head $ crossProduct (0::a) (0::b)

||| Magnitude of a Vect of Floats
||| This doesn't get to use the Num class because of the sqrt function
magnitude : Vect (S n) Float -> Float
magnitude v = sqrt $ dotProduct v v

||| Euclidean distance between two points
||| Doesn't get to use the Num class because of the sqrt in magnitude
distance : Vect (S n) Float -> Vect (S n) Float -> Float
distance a b = magnitude [| Classes.(-) a b |]

