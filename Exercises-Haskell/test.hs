import Data.List

longestEvenString :: String -> String
longestEvenString str = maximumBy' length $ filter (even . length) $ group str 
    where maximumBy' f = foldl1 (\m x -> if f x > f m then x else m)

generate :: (Num a, Enum a, Ord a) => (a -> a -> a) -> (a -> a -> a) -> a -> (a -> a)
generate f g n = product . (\x -> map (\i -> max (f i i) (g x x)) [0..n])

data Tree a = Empty | Node a (Tree a) (Tree a) a deriving (Show)

insertNode :: (Num a, Ord a) => a -> Tree a -> Tree a
insertNode value tree = helper tree 0
    where helper Empty depth = (Node value Empty Empty depth)
          helper (Node v left right depth) currentDepth
            | value > v = (Node v left (helper right (currentDepth + 1)) depth)
            | value < v = (Node v (helper left (currentDepth + 1)) right depth)
            | value == v = (Node v left right depth)


assignJobs :: Int -> [[Int]] -> Int
assignJobs k es = maximum $ map sum evaluatedPermutations
    where evaluatedPermutations = map (\perm -> map (\w -> imember w) perm) permWithPositions
          permWithPositions = map (\perm -> zip perm [1..k]) $ permutations es
          imember (xs, x) = if x `elem` xs then 1 else 0


combinations :: [[a]] -> [[a]]
combinations xss = foldl prod [[]] xss
    where prod yss xs = [ys ++ [x] | ys <- yss, x <- xs]

assignJobs' k es = maximum [length . nub $ c | c <- combinations es]

main = do
    print $ 