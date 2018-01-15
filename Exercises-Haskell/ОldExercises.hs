import Data.List
import Data.Ord
import Data.Function

qsort :: [Integer] -> [Integer]
qsort [] = []
qsort (x:xs) = smaller ++ [x] ++ larger
    where smaller = qsort $ filter (\y -> y <= x) xs
          larger = qsort $ filter (\y -> y > x) xs

choose :: (Floating a, Eq a) => a -> a -> a
n `choose` 0 = 1
n `choose` k = (n / k) * ((n - 1) `choose` (k - 1))

intToList :: Integer -> [Integer]	
intToList n
    | n == 0 = [0]
    | otherwise = toList n
        where toList 0 = []
              toList n = toList (n `div` 10) ++ [(n `mod` 10)] 

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

bullsAndCows :: Integer -> Integer -> (Int, Int)
bullsAndCows a b = (bulls, cows)
    where bulls = length $ filter (\x -> fst x == snd x) $ zip (intToList a) (intToList b)
          cows  = (length $ filter (\x -> x `elem` (intToList b)) (intToList a)) - bulls

countMinimum :: [Integer] -> Int
countMinimum xs = length $ filter (==(minimum xs)) xs

getPowerSet :: [Integer] -> [[Integer]]
getPowerSet = sortOn length . nub . concat . map inits . tails

getSublists :: [Integer] -> [[Integer]]
getSublists = sortOn length . concat . map inits . tails

countOccurances :: [Integer] -> [Integer] -> Int
countOccurances xs subxs = length $ filter (==subxs) (getSublists xs)

sieveOfEratosthenes :: Integer -> [Integer]
sieveOfEratosthenes n = sieve [2..n]
    where sieve [] = []  
          sieve (x:xs) = x : (sieve $ filter (\e -> e `mod` x /= 0) xs)

getMaxPower :: Integer -> Integer -> Integer
getMaxPower number base = helper 0
    where helper power = if number `mod` base^power /= 0 then power - 1 else helper (power + 1)

primeFactors :: Integer -> [(Integer, Integer)]
primeFactors n = map (\f -> (f, getMaxPower n f)) . filter (\x -> n `mod` x == 0) $ sieveOfEratosthenes n

dot :: (Num a) => [a] -> [a] -> a
v `dot` u = sum $ zipWith (*) v u 

sumUnique :: [[Integer]] -> Integer
sumUnique = sum . concat . filter ((==1) . length) . concat . map (group . sort)

transpose' :: [[Integer]] -> [[Integer]]
transpose' ([]:_) = []
transpose' matrix = (map head matrix) : transpose (map tail matrix)

eye :: Integer -> [[Integer]]
eye n = map (\x -> map (\y -> if x == y then 1 else 0) [1..n]) [1..n]

matrixProduct :: [[Integer]] -> [[Integer]] -> [[Integer]]
matrixProduct xss yss = map (\xs -> map (\ys -> (xs `dot` ys)) (transpose yss)) xss

catesianProduct :: [Integer] -> [Integer] -> [(Integer, Integer)]
catesianProduct xs ys = concat $ map (\x -> map (\y -> (x, y)) ys) xs

isMatrix :: [[Integer]] -> Bool
isMatrix = (==1) . length . nub . map length

permutations' :: [a] -> [[a]]
permutations' xs = foldl (\acc x -> concatMap (`insertEverywhere`x) acc) [[]] xs
    where insert' (xs, i) x = if i == 0 then x:xs else (head xs):(insert' ((tail xs), (i-1)) x)
          insertEverywhere xs x = map (\i -> insert' (xs, i) x) [0..(length xs)]

checkPermutation :: [Int] -> [Int] -> Bool
checkPermutation xs ys = (mapToPrimes xs) == (mapToPrimes ys)
    where mapToPrimes = product . map (\x -> sieve !! x)
          sieve = sieveOfEratosthenes 1000000000

countMinOccurances :: [Integer] -> Int
countMinOccurances = pred . length . groupBy ((==) `on` (>0))

subsets :: [a] -> [[a]]
subsets xs = foldr (\x yss -> yss ++ [x:ys | ys <- yss]) [[]] xs

argmax :: (Num a, Ord a, Ord b) => (a -> b) -> [a] -> a
argmax f = maximumBy (comparing f)

argmin :: (Num a, Ord a, Ord b) => (a -> b) -> [a] -> a
argmin f = minimumBy (comparing f)

main = do 
    print $ permutations' [1,2,3]