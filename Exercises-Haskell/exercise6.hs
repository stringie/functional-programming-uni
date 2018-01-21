import Data.List

splits :: [a] -> [([a],[a])]
splits xs = zip (inits xs) (tails xs)

increasing :: (Ord a, Enum a) => (a -> a) -> a -> a -> Bool
increasing f a b = and $ zipWith (<) fxs (tail fxs)
    where fxs = map f [a..b]

main = do
    print $ increasing (\x -> x/2) 1 10
