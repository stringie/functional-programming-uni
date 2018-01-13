{- C++ enum -}
-- Сезони
data Season = Spring | Summer | Autumn | Winter
    deriving (Read, Show)

-- Месеци
data Month = January | February | March | April | May | June | July | August | September | October | Novemer | December
    deriving (Read, Show)

{- C++ struct or (tagged) union -}
data Person = Person String Int
-- record syntax:
-- data Person = Person { name :: String, age :: Int }
--     deriving (Read, Show)
data Book = Book { bookTitle :: String, bookPublished :: Int, bookSales :: Int }
    deriving (Read, Show)

data Shape = Circle Double | Square Double 
    deriving (Read, Show)

{- C++ container classes -}
-- Списък от произволни елементи (от тип a)
data List a = Nil | Cons a (List a)
    deriving (Read, Show)

-- Tree a, описващ произволно двоично дърво, чийто стойности са от тип a.
data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Read, Show, Eq)


{- Примери -}
-- Дълбочина на двоично дърво:
treeDepth :: (Num b, Ord b) => Tree a -> b
treeDepth Empty = 0
treeDepth (Node _ left right) = 1 + max (treeDepth left) (treeDepth right)

-- Брой на листата на двоично дърво:
treeCountLeaves :: (Num b) => Tree a -> b
treeCountLeaves Empty = 0
treeCountLeaves (Node _ Empty Empty) = 1
treeCountLeaves (Node _ left right) = treeCountLeaves left + treeCountLeaves right

-- Сбор на всички стойности на върховете на двоично дърво:
treeSum :: Num a => Tree a -> a
treeSum Empty = 0
treeSum (Node x left right) = x + treeSum left + treeSum right

-- Списък със стойностите на всички вървове на ниво k:
nodesOnLevel :: (Integral b) => Tree a -> b -> [a]
nodesOnLevel Empty _ = []
nodesOnLevel (Node v left right) k
    | k < 0     = []
    | k == 0    = [v]
    | otherwise = nodesOnLevel left (k-1) ++ nodesOnLevel right (k-1)  


{- Задачи -}
{-
Задача 1. Дефинирайте функцията listSpecial tree, която приема двоично дърво tree
и връща списък от тези стойности на върховете на tree, които са равни на сбора на 
стойностите на децата си.

Примери:
    tree11 = (Node 3 
                (Node 1 
                    (Node 1 Empty Empty)
                    Empty)
                (Node 2 
                    (Node 4 Empty Empty)
                    (Node 7 Empty Empty)))
    
    listSpecial tree11 -> [3, 1]
-}

listSpecial :: (Eq a, Num a) => Tree a -> [a]
listSpecial Empty = []
listSpecial (Node x left right) = if x == (value left) + (value right) 
                                  then (if left == Empty && right == Empty then children else x:children) 
                                  else children
    where children = (listSpecial left) ++ (listSpecial right)
          value Empty = 0
          value (Node n _ _) = n


{-
Задача 2. Дефинирайте функцията areMirrorImages tree1 tree2, която приема две двойчни
дървета tree1 и tree2 и връща дали те са огледални образи едно на друго.

Примери:
    tree21 = (Node 4 
                (Node 3 
                    (Node 1 Empty Empty)
                    (Node 2 Empty Empty))
                (Node 5 
                    Empty
                    (Node 6 Empty Empty)))
    tree22 = (Node 4 
                (Node 5 
                    (Node 6 Empty Empty)
                    Empty)
                (Node 3 
                    (Node 2 Empty Empty)
                    (Node 1 Empty Empty)))
    tree23 = (Node 4 
                (Node 5 
                    (Node 6 Empty Empty)
                    Empty)
                (Node 3 
                    (Node 1 Empty Empty)
                    (Node 2 Empty Empty)))

    areMirrorImages tree21 tree22 -> True
    areMirrorImages tree21 tree23 -> False    
-}
areMirrorImages :: (Eq a) => Tree a -> Tree a -> Bool
areMirrorImages Empty Empty = True
areMirrorImages (Node x left1 right1) (Node y left2 right2) = x == y && (areMirrorImages left1 right2) && (areMirrorImages left2 right1)
                                                              


{-
Пордредени (сортирани) двоични дървета: 
 - Всеки вътрешен възел складира ключ (и по избор и стойност свъзрана с ключа).
 - Ключът във всеки възел трябва да е по-голям от всички ключове пазени влявото поддърво 
   и по-малък от всички ключове пазени в дясното поддърво.
-}

{-
Задача 3. Дефинирайте функцията isBST tree, която приема двоично дърво tree и връща
дали е подредено.

Примери:
    tree31 = (Node 4 
                (Node 2 
                    (Node 1 Empty Empty)
                    (Node 3 Empty Empty))
                (Node 5 
                    Empty
                    (Node 6 Empty Empty)))
    tree32 = (Node 4 
                (Node 2 
                    (Node 1 Empty Empty)
                    (Node 3 Empty Empty))
                (Node 5 
                    (Node 6 Empty Empty)
                    Empty))

    isBST tree31 -> True
    isBST tree32 -> False
-}
isBST :: (Ord a, Num a) => Tree a -> Bool
isBST tree = ordered $ bstToList tree
    where ordered list@(_:rest) = and $ zipWith (<) list rest
          ordered _ = True


{-
Задача 4. Дефнирайте функцията bstToList tree, която приема подредено 
двоично дърво tree и връща списък с ключовете на дървото подредени във
възходящ ред.

Примери: 
    tree   = (Node 4 
                (Node 2 
                    (Node 1 Empty Empty)
                    (Node 3 Empty Empty))
                (Node 5 
                    Empty
                    (Node 6 Empty Empty)))

    bstToList tree -> [1, 2, 3, 4, 5, 6]
-}
bstToList :: (Ord a) => Tree a -> [a]
bstToList Empty = []
bstToList (Node n left right) = bstToList left ++ [n] ++ bstToList right


{-
Задача 5. Дефинирайте функцията bstSearch tree value, която приема подредено
двоично дърво tree и стойност value и връща дали стойността се среща в дървото.

Примери:
    tree   = (Node 4 
                (Node 2 
                    (Node 1 Empty Empty)
                    (Node 3 Empty Empty))
                (Node 5 
                    Empty
                    (Node 6 Empty Empty)))

    bstSearch tree 1 -> True
    bstSearch tree 4 -> True
    bstSearch tree 7 -> False
-}
bstSearch :: (Ord a) => Tree a -> a -> Bool
bstSearch Empty _ = False
bstSearch (Node v left right) value 
    | v == value = True
    | v > value = bstSearch left value
    | v < value = bstSearch right value


{-
Задача 6. Дефинирайте функцията bstInsert tree value, която приема подредено
двоично дърво tree и стойност value и добавя value в дървото (като го оставя
наредено).

Примери:
    tree   = (Node 4 
                (Node 2 
                    (Node 1 Empty Empty)
                    (Node 3 Empty Empty))
                (Node 6 
                    Empty
                    (Node 7 Empty Empty)))

    bstInsert tree 7 -> (Node 4 
                            (Node 2 
                                (Node 1 Empty Empty) 
                                (Node 3 Empty Empty)) 
                            (Node 5 
                                Empty 
                                (Node 6 
                                    Empty 
                                    (Node 7 Empty Empty))))
-}
bstInsert :: (Ord a) => Tree a -> a -> Tree a
bstInsert Empty value = (Node value Empty Empty)
bstInsert (Node v left right) value
    | v == value = (Node v left right)
    | v > value = (Node v (bstInsert left value) right)
    | v < value = (Node v left (bstInsert right value))


{-
Задача 7. Дефинирайте функцията bstRemove tree value, която приема подредено
двоично дърво tree и стойност value и премахва value от дървото (като го оставя
наредено).

Примери:
    tree   = (Node 4 
                (Node 2 
                    (Node 1 Empty Empty)
                    (Node 3 Empty Empty))
                (Node 5 
                    Empty
                    (Node 6 Empty Empty)))

    bstRemove tree 4 -> (Node 2 
                            (Node 1 Empty Empty) 
                            (Node 3 
                                Empty 
                                (Node 5 
                                    Empty 
                                    (Node 6 Empty Empty))))
-}
bstRemove :: (Ord a) => Tree a -> a -> Tree a
bstRemove (Node v Empty Empty) _ = Empty
bstRemove (Node v left right) value
    | v == value = removeSubtree (Node v left right)
    | v > value = (Node v (bstRemove left value) right)
    | v < value = (Node v left (bstRemove right value))
        where removeSubtree (Node _ Empty right) = right
              removeSubtree (Node _ left Empty) = left
              removeSubtree (Node _ left right) = (Node leftest left (bstRemove right leftest))
                    where leftestElement (Node v Empty _) = v
                          leftestElement (Node _ left _) = leftestElement left
                          leftest = leftestElement right

-- main функция с примерни извиквания на функциите от задачите.
main :: IO()
main = let
    tree11 = (Node 3 
                (Node 1 
                    (Node 1 Empty Empty)
                    Empty)
                (Node 2 
                    (Node 4 Empty Empty)
                    (Node 7 Empty Empty)))
    tree21 = (Node 4 
                (Node 3 
                    (Node 1 Empty Empty)
                    (Node 2 Empty Empty))
                (Node 5 
                    Empty
                    (Node 6 Empty Empty)))
    tree22 = (Node 4 
                (Node 5 
                    (Node 6 Empty Empty)
                    Empty)
                (Node 3 
                    (Node 2 Empty Empty)
                    (Node 1 Empty Empty)))
    tree23 = (Node 4 
                (Node 5 
                    (Node 6 Empty Empty)
                    Empty)
                (Node 3 
                    (Node 1 Empty Empty)
                    (Node 2 Empty Empty)))
    tree31 = (Node 4 
                (Node 2 
                    (Node 1 Empty Empty)
                    (Node 3 Empty Empty))
                (Node 5 
                    Empty
                    (Node 6 Empty Empty)))
    tree32 = (Node 4 
                (Node 2 
                    (Node 1 Empty Empty)
                    (Node 3 Empty Empty))
                (Node 5 
                    (Node 6 Empty Empty)
                    Empty))
    -- Зад. 4-7.
    tree   = (Node 4 
                (Node 2 
                    (Node 1 Empty Empty)
                    (Node 3 Empty Empty))
                (Node 5 
                    Empty
                    (Node 6 Empty Empty)))
    in do
        -- Задача 1.
        print $ listSpecial tree11

        -- Задача 2.
        print $ areMirrorImages tree21 tree22
        print $ areMirrorImages tree21 tree23

        -- Задача 3.
        print $ isBST tree31
        print $ isBST tree32

        -- Задача 4.
        print $ bstToList tree

        -- Задача 5.
        print $ bstSearch tree 1
        print $ bstSearch tree 4
        print $ bstSearch tree 7
        
        -- Задача 6.
        print $ bstInsert tree 7

        -- Задача 7.
        print $ bstRemove (Node 2 (Node 1 Empty Empty) (Node 4 (Node 3 Empty Empty) (Node 5 Empty Empty))) 4    