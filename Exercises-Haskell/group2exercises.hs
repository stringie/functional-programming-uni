data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

sumNodes :: (Num a) => Tree a -> a
sumNodes Empty = 0
sumNodes (Node v left right) = v + sumNodes left + sumNodes right

bstInsert :: (Ord a) => Tree a -> a -> Tree a
bstInsert Empty value = (Node value Empty Empty)
bstInsert (Node v left right) value
    -- | v == value = (Node v left right)
    | v >= value = (Node v (bstInsert left value) right)
    | v < value = (Node v left (bstInsert right value))

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node v l r) = inOrder l ++ [v] ++ inOrder r

treeSort :: (Ord a) => [a] -> [a]
treeSort xs = inOrder $ foldl bstInsert Empty xs

main = do
    print $ inOrder (Node 1 (Node 2 (Node 3 Empty Empty) (Node 5 Empty Empty)) (Node 4 Empty Empty))
    print $ treeSort [1,6,5,3,7,5,6,6,7,8,8,3,2]