import Data.List

type Cell = (Integer, Integer)
type Board = [Cell]

gameOfLife :: Board -> Int -> Board
gameOfLife board n = iterate tick (sort board) !! n

tick :: Board -> Board
tick board = [c | gs@(c:cs) <- groups, length gs == 3 || (length gs == 2 && c `elem` board)]
    where groups = group $ sort $ concatMap neighbors board

neighbors :: Cell -> [Cell]
neighbors (x, y) = [((x + i), (y + j)) | i <- [-1 .. 1], j <- [-1 .. 1], (i, j) /= (0,0)]

main :: IO()
main = let board = [(2, 1), (0, 1), (1, 2), (2, 0), (2, 2)]
    in do
        print $  group $ sort $ concatMap neighbors board