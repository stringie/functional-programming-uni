data Vector a = Vector a a a deriving (Show)

vplus :: Num a => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = (Vector (i+l) (m+j) (k+n))

main = do
    print $ (Vector 1 2.0 3) `vplus` (Vector 3 2 1)
