factoriel :: Integer -> Integer
factoriel n = product [1..n]


main = do
    print (factoriel 2)