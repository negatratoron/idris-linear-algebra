import Tensor
import Tensor2


-- looks like a row vector, actually a column vector
vector : Vect 3 Int
vector = [0, 4, 2]

matrix : Matrix 2 3 Int
matrix = [[0, 4, 3]
         ,[1, 3, 8]]

vector2 : Vect 2 Int
vector2 = multVect matrix vector


main : IO ()
main = putStrLn (show vector2)
