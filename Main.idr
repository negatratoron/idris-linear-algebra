import Tensor
import Tensor2


-- looks like a row vector, actually a column vector
vector : Vec 3 Int
vector = MkVector [0, 4, 2]


vector2 : Vec 3 Int
vector2 = MkVector [2, 4, 1]


product : Scalar Int
product = dotProduct vector vector2


matrix : Matrix 2 3 Int
matrix = MkVector [MkVector [0, 4, 3]
                  ,MkVector [1, 3, 8]
                  ]

vector3 : Vec 2 Int
vector3 = multVec matrix vector



main : IO ()
--main = putStrLn $ show Main.product
main = putStrLn (show vector3)
