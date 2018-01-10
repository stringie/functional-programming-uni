import Data.List
import Data.Ord
import Data.Function

reverseOrdStuff n = helper 0 n
    where helper res num
            | num == 0 = res
            | last >= rest `mod` 10 = (10 * res) + last
            | otherwise = helper (10 * res + last) rest
                where last = (num `mod` 10)
                      rest = (num `div` 10)


sumUnique xss = sum $ concat $ filter ((==1) . length) $ concatMap (group . sort) xss

argmin :: (Ord a, Ord b) => (a -> b) -> [a] -> a
argmin f = minimumBy (comparing f)

type Product = (String, Double)
type StoreAvailability = [Product]
store1 = [("bread", 1), ("milk", 2.5), ("lamb", 10), ("cheese", 5), ("butter", 2.3)]
store2 = [("bread", 1), ("cheese", 2.5), ("bread", 1), ("cheese", 5), ("butter", 2.3)]

closestToAverage avail = fst $ argmin (\(s, d) -> abs (average - d)) avail
    where average = (sum $ map snd avail) / (fromIntegral $ length avail)

cheaperAlternative avail = sum differentByPrice
    where groupByProduct = groupBy ((==) `on` fst) $ sort avail
          longerThanOne = filter ((>1) . length) groupByProduct
          differentByPrice = map (\g -> if (length $ nub $ map snd g) > 1 then 1 else 0) longerThanOne

pointsArr = [(1,1,1),(2,2,2),(3,3,3)]

minDistance points = minimum $ map distance allPairs
    where distance [(x1, y1, z1), (x2, y2, z2)] = (x1 - x2)^2 + (y1-y2)^2 + (z1 - z2)^2
          allPairs = filter ((==2) . length) $ subsequences points
    
functions = [(\x -> x*x*x), (\x -> x+1)]

maximize fs = (\x -> maximumBy (comparing abs) $ map (\f -> f x) fs)

inverseFun f g a b = and $ map (\x -> (f . g) x == x && (g . f) x == x ) [a..b]

main = do
    print $ inverseFun (\x -> x+1) (\x -> x+2) 0 1  