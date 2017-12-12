sort :: [Integer] -> [Integer]
sort [] = []
sort (x:xs) = smaller ++ [x] ++ larger
    where smaller = sort $ filter (\y -> y <= x) xs
          larger = sort $ filter (\y -> y > x) xs

bc :: (Floating a, Eq a) => a -> a -> a
bc n 0 = 1
bc n k = (n / k) * (bc (n - 1) (k - 1))

intToList :: Integer -> [Integer]
intToList 0 = []
intToList n = intToList (n `div` 10) ++ [n `mod` 10]

listToInt :: [Integer] -> Integer
listToInt = foldl1 (\acc x-> (acc * 10) + x)

reverseNum :: Integer -> Integer
reverseNum = listToInt . reverse . intToList

isPrime :: Integer -> Bool
isPrime n = and $ map (\x -> n `mod` x /= 0) [2..(n-1)]

sumPrimes :: Integer -> Integer -> Integer
sumPrimes a b = sum $ filter isPrime [a..b]

isPerfectNumber :: Integer -> Bool
isPerfectNumber n = (==) n $ sum $ filter (\x -> n `mod` x == 0) [1..(n - 1)]

hasAscendingDigits :: Integer -> Bool
hasAscendingDigits n = intToList n == (sort $ intToList n)

main = do 
    print $ hasAscendingDigits 121