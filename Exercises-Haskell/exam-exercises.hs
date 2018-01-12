import Data.List
import Data.Ord
import Data.Function

reverseOrdStuff :: Integer -> Integer
reverseOrdStuff n = helper 0 n
    where helper res num
            | num == 0 = res
            | last >= rest `mod` 10 = (10 * res) + last
            | otherwise = helper (10 * res + last) rest
                where last = (num `mod` 10)
                      rest = (num `div` 10)


sumUnique :: [[Integer]] -> Integer                    
sumUnique xss = sum $ concat $ filter ((==1) . length) $ concatMap (group . sort) xss

argmin :: (Ord a, Ord b) => (a -> b) -> [a] -> a
argmin f = minimumBy (comparing f)

type Product = (String, Double)
type StoreAvailability = [Product]
store1 = [("bread", 1), ("milk", 2.5), ("lamb", 10), ("cheese", 5), ("butter", 2.3)]
store2 = [("bread", 1), ("cheese", 2.5), ("bread", 1), ("cheese", 5), ("butter", 2.3)]

closestToAverage :: StoreAvailability -> String
closestToAverage avail = fst $ argmin (\(s, d) -> abs (average - d)) avail
    where average = (sum $ map snd avail) / (fromIntegral $ length avail)

cheaperAlternative :: StoreAvailability -> Double
cheaperAlternative avail = sum differentByPrice
    where groupByProduct = groupBy ((==) `on` fst) $ sort avail
          longerThanOne = filter ((>1) . length) groupByProduct
          differentByPrice = map (\g -> if (length $ nub $ map snd g) > 1 then 1 else 0) longerThanOne

pointsArr = [(1,1,1),(2,2,2),(3,3,3)]

minDistance :: [(Double, Double, Double)] -> Double
minDistance points = minimum $ map distance allPairs
    where distance [(x1, y1, z1), (x2, y2, z2)] = (x1 - x2)^2 + (y1-y2)^2 + (z1 - z2)^2
          allPairs = filter ((==2) . length) $ subsequences points
    
functions = [(\x -> x*x*x), (\x -> x+1)]

maximize :: (Ord a, Num a) => [(a -> a)] -> (a -> a)
maximize fs = (\x -> maximumBy (comparing abs) $ map (\f -> f x) fs)

inverseFun :: (Eq a, Enum a) => (a -> a) -> (a -> a) -> a -> a -> Bool
inverseFun f g a b = and $ map (\x -> (f . g) x == x && (g . f) x == x ) [a..b]

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read)

mirror :: Tree a -> Tree a
mirror Empty = Empty
mirror (Node v left right) = (Node v (mirror right) (mirror left))  

getLevels :: Tree a -> [(a, Int)]
getLevels tree = helper [(tree, 0)] 
    where helper [] = []
          helper ((Empty, _):xs) = helper xs
          helper (((Node v left right), level):xs) = (v, level) : helper (xs ++ [(left, (level + 1)), (right, (level + 1))])

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node v left right) = inOrder left ++ [v] ++ inOrder right

main = do
    print $ getLevels (Node 1 (Node 2 Empty (Node 7 Empty (Node 8 Empty Empty))) (Node 3 (Node 5 Empty (Node 6 Empty Empty)) (Node 4 Empty Empty)))