data Cell = Cell { x :: Integer, y :: Integer} deriving (Show)
type board = [Cell]

tic board = 

gameOfLife board 0 = board 
gameOfLife board n = gameOfLife (tic board) (n-1)