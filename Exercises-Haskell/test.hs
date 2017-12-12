solveRPN :: (Num a, Read a) => String -> a
solveRPN = head . foldl folding [] . words
    where folding (x:y:ys) "+" = (x+y):ys
          folding (x:y:ys) "-" = (x-y):ys
          folding (x:y:ys) "*" = (x*y):ys
          folding xs numberString = read numberString:xs
main = do
    print $ solveRPN "10 20 +"
    