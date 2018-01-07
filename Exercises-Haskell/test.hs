import Data.List

gameOfLife :: [(Integer,Integer)] -> [(Integer,Integer)] 
gameOfLife liveCells = let all = allCells ((fst (head liveCells)) - 1 ) ((snd (head liveCells)) - 1 ) ((fst (last liveCells)) + 1) ((snd (last liveCells)) +1) ((snd (head liveCells)) - 1 )
                           dead = deadCells all liveCells
                           deadToLive = deadToLiveCells (fst (head dead)) (snd (head dead))  (tail dead)  liveCells
                           live = newLiveCells (fst (head liveCells)) (snd (head liveCells))  (tail liveCells) liveCells
                        in sort (deadToLive ++ live)
    where 
        allCells :: Integer->Integer -> Integer -> Integer -> Integer -> [(Integer,Integer)]
        allCells x0 y0 xn yn copyY0
         | x0 == xn && y0 == (yn +1)  = []
         | y0 /= (yn +1) =  (x0,y0) : (allCells x0 (y0 + 1) xn yn copyY0)
         | otherwise = allCells (x0 + 1) copyY0 xn yn copyY0

        deadCells ::  [(Integer,Integer)] -> [(Integer,Integer)] -> [(Integer,Integer)] 
        deadCells all liveCells
          | null all = []
          | (head all) `elem` liveCells = deadCells (tail all) liveCells
          | otherwise = (head all) : deadCells (tail all) liveCells

        deadToLiveCells :: Integer -> Integer ->  [(Integer,Integer)]-> [(Integer,Integer)] -> [(Integer,Integer)] 
        deadToLiveCells x y dead liveCells
         | null dead && ((countNeighbors x y liveCells 0) == 3) = (x,y) : []
         | null dead = []
         | (countNeighbors x y liveCells 0) == 3 = (x,y) : deadToLiveCells (fst (head dead)) (snd (head dead)) (tail dead) liveCells
         | otherwise = deadToLiveCells (fst (head dead)) (snd (head dead)) (tail dead) liveCells
           where
               countNeighbors :: Integer -> Integer ->  [(Integer,Integer)] -> Integer -> Integer
               countNeighbors x y liveCells counter
                | null liveCells = counter
                | (fst (head liveCells)) == (x - 1) && (snd (head liveCells)) == (y - 1) = countNeighbors x y (tail liveCells) (counter + 1)
                | (fst (head liveCells)) == (x - 1) && (snd (head liveCells)) == y  = countNeighbors x y (tail liveCells) (counter + 1)
                | (fst (head liveCells)) == (x - 1) && (snd (head liveCells)) == (y + 1) = countNeighbors x y (tail liveCells) (counter + 1)
                | (fst (head liveCells)) == x  && (snd (head liveCells)) == (y - 1) = countNeighbors x y (tail liveCells) (counter + 1)
                | (fst (head liveCells)) == x  && (snd (head liveCells)) == (y + 1) = countNeighbors x y (tail liveCells) (counter + 1)
                | (fst (head liveCells)) == (x + 1) && (snd (head liveCells)) == (y - 1) = countNeighbors x y (tail liveCells) (counter + 1)
                | (fst (head liveCells)) == (x + 1) && (snd (head liveCells)) == y  = countNeighbors x y (tail liveCells) (counter + 1)
                | (fst (head liveCells)) == (x + 1) && (snd (head liveCells)) == (y + 1) = countNeighbors x y (tail liveCells) (counter + 1)
                | otherwise = countNeighbors x y (tail liveCells) counter 
        
        newLiveCells :: Integer -> Integer ->  [(Integer,Integer)]-> [(Integer,Integer)] -> [(Integer,Integer)] 
        newLiveCells x y liveCells copyLiveCells
         | null liveCells && ((countNeighbors x y copyLiveCells 0) == 2 || (countNeighbors x y copyLiveCells 0) == 3) = (x,y) : []
         | null liveCells = []
         | (countNeighbors x y copyLiveCells 0) == 2 = (x,y) : newLiveCells (fst (head liveCells)) (snd (head liveCells)) (tail liveCells) copyLiveCells
         | (countNeighbors x y copyLiveCells 0) == 3 = (x,y) : newLiveCells (fst (head liveCells)) (snd (head liveCells)) (tail liveCells) copyLiveCells
         | otherwise = newLiveCells (fst (head liveCells)) (snd (head liveCells)) (tail liveCells) copyLiveCells
            where
                countNeighbors :: Integer -> Integer ->  [(Integer,Integer)] -> Integer -> Integer
                countNeighbors x y copyLiveCells counter
                 | null copyLiveCells = counter
                 | (fst (head copyLiveCells)) == (x - 1) && (snd (head copyLiveCells)) == (y - 1) = countNeighbors x y (tail copyLiveCells) (counter + 1)
                 | (fst (head copyLiveCells)) == (x - 1) && (snd (head copyLiveCells)) == y  = countNeighbors x y (tail copyLiveCells) (counter + 1)
                 | (fst (head copyLiveCells)) == (x - 1) && (snd (head copyLiveCells)) == (y + 1) = countNeighbors x y (tail copyLiveCells) (counter + 1)
                 | (fst (head copyLiveCells)) == x  && (snd (head copyLiveCells)) == (y - 1) = countNeighbors x y (tail copyLiveCells) (counter + 1)
                 | (fst (head copyLiveCells)) == x  && (snd (head copyLiveCells)) == (y + 1) = countNeighbors x y (tail copyLiveCells) (counter + 1)
                 | (fst (head copyLiveCells)) == (x + 1) && (snd (head copyLiveCells)) == (y - 1) = countNeighbors x y (tail copyLiveCells) (counter + 1)
                 | (fst (head copyLiveCells)) == (x + 1) && (snd (head copyLiveCells)) == y  = countNeighbors x y (tail copyLiveCells) (counter + 1)
                 | (fst (head copyLiveCells)) == (x + 1) && (snd (head copyLiveCells)) == (y + 1) = countNeighbors x y (tail copyLiveCells) (counter + 1)
                 | otherwise = countNeighbors x y (tail copyLiveCells) counter 
                  
main = do
    print $ gameOfLife [(0,1),(1,2),(2,0),(2,1),(2,2)]
    print $ gameOfLife [(0,0),(0,1),(1,0),(1,1)]
    print $ gameOfLife [(0,-1),(0,0),(0,1)]