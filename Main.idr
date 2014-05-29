import Matrix


-- looks like a row vector, actually a column vector
vector : Vect 3 Int
vector = [0, 4, 2]

matrix : Matrix 2 3 Int
matrix = [[0, 4, 3]
         ,[1, 3, 8]]

vector2 : Vect 2 Int
vector2 = multVect matrix vector


vect : Vect 3 Int
vect = [3, 4, 5]

vect2 : Vect 4 Int
vect2 = insertAt 0 6 vect

vect3 : Vect 4 Int
vect3 = insertAt 2 6 vect


main : IO ()
main = do
  putStrLn (show vect2)
  putStrLn (show vect3)
