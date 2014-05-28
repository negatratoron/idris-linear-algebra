import Tensor
import Tensor2


-- looks like a row vector, actually a column vector
vector : Vector 3 Int
vector = MkVector [0, 4, 2]

matrix : Matrix 2 3 Int
matrix = MkVector [MkVector [0, 4, 3]
                  ,MkVector [1, 3, 8]
                  ]

vector2 : Vector 2 Int
vector2 = multVector matrix vector


main : IO ()
main = putStrLn (show vector2)
