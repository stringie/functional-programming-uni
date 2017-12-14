import Data.List

data Cell = Cell { x :: Integer, y :: Integer} deriving (Show, Eq, Ord)
type Board = [Cell]

gameOfLife :: (Integral n) => Board -> n -> Board
gameOfLife board n = 
    if (n <= 0) 
        then board 
        else gameOfLife (tick board) (n-1)

tick :: Board -> Board
tick board = [c | gs@(c:cs) <- groups, length gs == 3 || (length gs == 4 && c `elem` board)]
    where groups = group $ sort $ concatMap neighborsAndSelf board

neighborsAndSelf :: Cell -> [Cell]
neighborsAndSelf (Cell x y) = [Cell (x + i) (y + j) | i <- [-1 .. 1], j <- [-1 .. 1]]

main :: IO()
main = let board = map (uncurry Cell) [(0, 1), (1, 2), (2, 0), (2, 1), (2, 2)]
    in do
        print $ gameOfLife board 3