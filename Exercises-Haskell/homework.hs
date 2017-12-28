import Data.List

intToList :: Integer -> [Integer]
intToList 0 = []
intToList n = intToList (n `div` 10) ++ [n `mod` 10]

calcLuhnChecksum :: Integer -> Integer
calcLuhnChecksum n = (9 * performSum) `mod` 10
    where evenIndexMapToDouble = map (\(i, x) -> if even i then 2*x else x) $ zip [1..] $ intToList n
          performSum = sum $ concatMap intToList evenIndexMapToDouble

--------------------------------------------------------------------
gameOfLife :: [(Integer,Integer)] -> [(Integer, Integer)]
gameOfLife board = [c | gs@(c:cs) <- groups, length gs == 3 || (length gs == 4 && c `elem` board)]
    where groups = group $ sort $ concatMap neighborsAndSelf board

neighborsAndSelf :: (Integer, Integer) -> [(Integer, Integer)]
neighborsAndSelf (x, y) = [((x + i), (y + j)) | i <- [-1 .. 1], j <- [-1 .. 1]]

---------------------------------------------------------------------
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read)

isBalanced :: Tree a -> Integer -> Bool
isBalanced Empty _ = True
isBalanced (Node _ left right) k = heightDifference <= k && balancedLeft && balancedRight
    where heightDifference = abs ((height left) - (height right))
          balancedLeft = isBalanced left k
          balancedRight = isBalanced right k

height :: Tree a -> Integer   
height Empty = -1
height (Node _ left right) 
    | leftHeight >= rightHight = leftHeight + 1
    | otherwise = rightHight + 1
        where leftHeight = height left
              rightHight = height right 
