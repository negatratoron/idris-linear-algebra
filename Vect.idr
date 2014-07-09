module Vect

%access public

dotProduct : (Num a) => Vect (S n) a -> Vect (S n) a -> a
dotProduct a b = foldr1 (+) $ zipWith (*) a b


crossProduct : (Num a) => Vect 3 a -> Vect 3 a -> Vect 3 a
crossProduct [a1, a2, a3] [b1, b2, b3] = [a2*b3 - a3*b2, a3*b1 - a1*b3, a1*b2 - b1*a2]


crossProduct2 : (Num a) => Vect 2 a -> Vect 2 a -> a
crossProduct2 a b = head $ crossProduct (0::a) (0::b)


