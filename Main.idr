import Tensor
import Tensor2


vector : Vec 3 Int
vector = Vector [0, 4, 2]

-- 3 cols, 3 rows
-- the literal notation is crooked, which is very bad
matrix : Matrix 3 3 Int
matrix = Vector [Vector [0, 4, 3]
                ,Vector [1, 3, 8]
                ,Vector [8, 2, 1]
                ]

vector2 : Vec 3 Int
vector2 = multVec matrix vector



main : IO ()
main = let (Vector v) = vector2 in putStrLn (show v)
