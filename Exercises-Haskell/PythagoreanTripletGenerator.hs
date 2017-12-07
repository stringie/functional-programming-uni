-- https://en.wikipedia.org/wiki/Tree_of_primitive_Pythagorean_triples

dotProduct :: [Integer] -> [Integer] -> Integer
dotProduct v u = sum $ zipWith (*) v u

matrixVectorProduct :: [[Integer]] -> [Integer] -> [Integer]
matrixVectorProduct m v = map (\r -> dotProduct r v) m
 
branch :: [Integer] -> [[Integer]]
branch triplet = map (\m -> matrixVectorProduct m triplet) [a, c, b]
    where a = [[1,-2,2], [2,-1,2], [2,-2,3]]
          b = [[1,2,2], [2,1,2], [2,2,3]]
          c = [[-1,2,2], [-2,1,2], [-2,2,3]]

pythagoranTree :: [[Integer]] -> [[Integer]]
pythagoranTree series = head series : (pythagoranTree updatedRest)
    where updatedRest = tail series ++ (branch $ head series)

main :: IO ()
main = do
    print $ take 10 (pythagoranTree [[3,4,5]])