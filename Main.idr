import Tensor
import Tensor2


-- looks like a row vector, actually a column vector
vector : Vec 3 Int
vector = Vector [0, 4, 2]


vector2 : Vec 3 Int
vector2 = Vector [2, 4, 1]


product : scalar Int
product = dotProduct vector vector2


matrix : Matrix 3 2 Int
matrix = Vector [Vector [0, 4, 3]
                ,Vector [1, 3, 8]
                ]

vector3 : Vec 2 Int
vector3 = multVec matrix vector



main : IO ()
--main = putStrLn $ show Main.product
main = let (Vector v) = vector3 in putStrLn (show v)
