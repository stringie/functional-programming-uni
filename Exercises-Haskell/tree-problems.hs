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
          helper xs = map value xs ++ helper (concatMap leftAndRight xs)
          value (Node a _ _) = a
          leftAndRight (Node _ Empty Empty) = []
          leftAndRight (Node _ l Empty) = [l]
          leftAndRight (Node _ Empty r) = [r]
          leftAndRight (Node _ l r) = [l, r]

main = do
    print $ levelOrder (Node 1 (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty)) (Node 5 (Node 6 Empty Empty) Empty))