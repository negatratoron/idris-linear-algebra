import Tensor
import Tensor2


zero : Tensor 0 Nil Int
zero = Scalar 0

one : Tensor 0 Nil Int
one = Scalar 1

two : Tensor 0 Nil Int
two = Scalar 2

three : Tensor 0 Nil Int
three = Scalar 3

four : Tensor 0 Nil Int
four = Scalar 4

five : Tensor 0 Nil Int
five = Scalar 5

six : Tensor 0 Nil Int
six = Scalar 6


playerCoords : Tensor 1 [3] Float
playerCoords = Vector [Scalar 38.0, Scalar 815.2, Scalar 3289.12]


vector : Vec 3 Int
vector = Vector [zero, four, two]

-- 3 cols, 3 rows
-- the literal notation is crooked, which is very bad
matrix : Matrix 3 3 Int
matrix = Vector [Vector [zero, four, three]
                ,Vector [one, four, three]
                ,Vector [two, two, three]
                ]

vector2 : Vec 3 Int
vector2 = multVec matrix vector



main : IO ()
--main = putStrLn (show (transpose example2))
main = let (Vector v) = vector2 in putStrLn (show v)
