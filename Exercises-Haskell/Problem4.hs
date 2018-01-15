import Data.List

assignJobs k es = maximum $ map sum evaluatedPermutations
    where evaluatedPermutations =  map (\perm -> map (\w -> imember w) perm) $ map (\perm -> zip perm [1..k]) kTakenPermutations
          imember (xs, x) = if x `elem` xs then 1 else 0
          kTakenPermutations = map (k`take`) $ permutations es

main = do
    print $ assignJobs 3 [[1,2],[2],[1,3],[1]]