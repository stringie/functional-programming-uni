quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = smaller ++ [x] ++ larger
    where smaller = quicksort [a | a <- xs, a <= x]
          larger = quicksort [a | a <- xs, a > x]

main = do
    print (quicksort [5,6,3,4,6,7,8,9,1,2,3,2,4,6,7,4])