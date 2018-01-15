import Data.Char  -- за isAlpha, isDigit
import Data.List hiding (maximumBy) -- за nub

maximumBy :: (Ord b) =>  (a -> b) -> [a] -> a
maximumBy f xs = foldr1 (\x maxi -> if f x > f maxi then x else maxi) xs

-- Задача 1.
longestSubs' :: String -> String 
longestSubs' str = maximumBy length [s | s <- splitNumbers str, even (read s)] where
    splitNumbers "" = []
    splitNumbers s@(c:cs)
        | isDigit c = takeWhile isDigit s : splitNumbers (dropWhile isDigit s)
        | otherwise = splitNumbers cs

longestSubs'' :: String -> String
longestSubs'' str = maximumBy length [s | s <- splitSame str, even (length s)]where
    splitSame "" = []
    splitSame s@(c:cs)
        | isAlpha c = takeWhile (== c) s : splitSame (dropWhile (== c) cs) 
        | otherwise = splitSame cs


-- Задача 2.
generate' :: (Num a, Integral b) => (a -> Bool) -> (a -> Bool) -> b -> a -> Bool
generate' f g n x = (g x) && or [f i | i <- map fromIntegral [0..n]] 

generate'' :: (Num a, Ord b, Num b, Integral c) => (a -> a -> b) -> (a -> a -> b) -> c -> a -> b
generate'' f g n x = product [max (f i i) gxx | i <- map fromIntegral [0..n]] 
    where gxx = g x x


-- Задача 3.
-- Вторият Int държи:
-- или "разликата между броя на върховете в лявото и дясното поддървета"
-- или "броя на върховете до корена"
data Tree = Empty | Node Int Int Tree Tree
    deriving (Read, Show)

insert' :: Int -> Tree -> Tree
insert' val tree = helper val 0 tree where
    helper val h Empty = Node val h Empty Empty
    helper val h tree@(Node v n left right) 
        | val <  v = Node v n (helper val (h+1) left) right 
        | val >  v = Node v n left (helper val (h+1) right)
        | val == v = tree

insert'' :: Int -> Tree -> Tree
insert'' val Empty = Node val 0 Empty Empty
insert'' val tree@(Node v dlr left right)
    | val <  v = Node v (dlr+1) (insert'' val left) right
    | val >  v = Node v (dlr-1) left (insert'' val right)
    | val == v = tree


-- Задача 4.
-- упр. 13 зад. 4. 
combinations :: [[a]] -> [[a]]
combinations xss = foldr prod [[]] xss where
    prod xs yss = [x:ys | x <- xs, ys <- yss]

-- пример: findMinTime 2 [1, 3, 4, 12, 17] -> [[1, 17], [3, 4, 12]] -> 19
findMinTime :: Int -> [Int] -> Int
findMinTime k ds = minimum [calcTime assignment | assignment <- combinations (replicate n machines)] where
    n = length ds
    machines = [1..k]
    calcTime assignment = maximum [sum [t | (a, t) <- ats, a == m] | m <- machines] where
        ats = zip assignment ds

-- Решение на Александър Шумаков
findMinTime' :: Int -> [Int] -> Int
findMinTime' k ds = helper (replicate k 0) ds where
    -- алокирали сме всички задачи: времето за завършване е най-дългото време за работа на някоя от машините 
    helper times [] = maximum times
    helper times (d:ds) = 
        -- генерираме всички възможни алокации на задача d и оптимизираме рекурсивно за всяка от тях 
        let allocationss = zipWith (\ts i -> addTime d ts i) (replicate k times) [1..k]
        in minimum $ [helper newTimes ds | newTimes <- allocationss]
    -- добавяме времето t на машина i (т.е. алокираме детаила на машина i).
    addTime t (x:xs) 1 = (x+t):xs
    addTime t (x:xs) n = x:addTime t xs (n-1)


-- пример: assignJobs 3 [[1], [2], [1, 2]] -> 2 
-- пример: assignJobs 3 [[1], [2], [1, 2, 3]] -> 3
-- по-смислено решение тук: https://www.geeksforgeeks.org/maximum-bipartite-matching/
assignJobs :: Int -> [[Int]] -> Int
assignJobs _ skillss = maximum $ [length $ nub c | c <- combinations skillss]

-- Елегантно решение на Антони Калоферов
assignJobs' :: Int -> [[Int]] -> Int
assignJobs' k es = maximum $ map countValidAssignments (permutations es) where
    countValidAssignments perm =  length [employee | (job, employee) <- zip jobs perm, canAssign job employee]
    jobs = [1..k]
    canAssign job employee = job `elem` employee