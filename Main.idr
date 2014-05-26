import Tensor


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


example : Tensor 1 [3] Int
example = Vector [zero, four, two]

example2 : Tensor 2 [2, 3] Int
example2 = Vector [Vector [zero, four, two]
                  ,Vector [one, four, two]
                  ]



main : IO ()
main = putStrLn (show (transpose example2))
