import System.IO  
import System.Directory  
import Data.List  

main = do
    handle <- openFile "todo.txt" ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let tasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] tasks
    putStrLn "These are your todos:"
    putStr $ unlines numberedTasks
    putStrLn "What will you delete?"
    numberString <- getLine
    let number = read numberString
        newTodo = delete (tasks !! number) tasks
    hPutStr tempHandle $ unlines newTodo
    hClose handle
    hClose tempHandle
    removeFile "todo.txt"
    renameFile tempName "todo.txt"