quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = smaller ++ [x] ++ larger 
    where smaller = quicksort [a | a <- xs, a <= x]
          larger = quicksort [a | a <- xs, a > x]

main = do
    print (quicksort [6, 4, 2, 7, 8, 4, 6, 4, 1, 9, 3, 4, 5, 6])