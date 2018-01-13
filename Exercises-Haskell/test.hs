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

subsets :: [a] -> [[a]]
subsets xs = foldr (\x yss -> yss ++ [x:ys | ys <- yss]) [[]] xs

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = null [ x | x <- [2..n-1], n `mod` x == 0]

filterPrimePosition xs = [x | (x, i) <- zip xs [2..], isPrime i]

sumNumbers :: String -> Int
sumNumbers s = sum [(read x :: Int) | x <- groups, (isDigit . head) x]
    where isDigit c = c <= '9' && c >= '0'
          groups = groupBy (\x y -> isDigit x && isDigit y) s
        --   toDigit c = c - '0'

main = do
    print $ sumNumbers "abc123d4"