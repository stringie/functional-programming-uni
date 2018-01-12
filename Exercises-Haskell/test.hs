import Data.List

reverseIthColumn :: [[Integer]] -> Int -> [[Integer]]
reverseIthColumn xss i = transpose (reverseIthRow (transpose xss) i)
    where reverseIthRow xss i = map (\(col, pos) -> if pos == i then reverse col else col) 
                                $ zip xss [1..(length xss)]

diagonalProduct :: [[Integer]] -> Integer                                
diagonalProduct xss = sum $ zipWith (*) mainDiag secondDiag
    where mainDiag = zipWith (\xs i -> xs !! i) xss [0..len-1] 
          secondDiag = zipWith (\xs i -> xs !! i) xss [len-1, len-2..0]
          len = length xss

main = do
    print $ diagonalProduct [[1,2,3],[4,5,6],[7,8,9]]