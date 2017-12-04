oddSquareSum :: Integer
oddSquareSum = sum $ takeWhile (<1000) $ filter odd $ map (^2) [1..]


main = do
    print $ oddSquareSum