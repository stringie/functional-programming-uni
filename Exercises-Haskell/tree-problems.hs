data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read)

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node v left right) = inOrder left ++ [v] ++ inOrder right

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node v left right) = v : preOrder left ++ preOrder right

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node v left right) = postOrder left ++ postOrder right ++ [v]

levelOrder :: Tree a -> [a]
levelOrder tree = helper [tree]
    where helper [] = []
          helper (Empty:xs) = helper xs
          helper ((Node v l r):xs) = v : helper (xs ++ [l,r])

main = do
    print $ levelOrder (Node 1 (Node 2 (Node 4 Empty Empty) Empty) (Node 3 Empty Empty))