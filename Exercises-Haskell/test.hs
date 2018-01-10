fibs = scanl (+) 0 (1:fibs)

main = do
    print $ fibs !! 100000