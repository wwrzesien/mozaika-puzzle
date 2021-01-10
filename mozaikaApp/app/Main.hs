module Main where

import Solver
import Display

-- checkRow::String -> 

-- solver
-- solver::[String] -> [String]
-- solver [] = return ()
-- solver (x:xs) = do
--     checkRow x
--     checkRow xs

main :: IO ()
main = do 
    puzzle <- readPuzzle "puzzles/puzzle01.txt"
    putStrLn ("Łamigłówka:")
    printPuzzle puzzle -- wyświetl łamigłówkę

    -- let output = solver puzzle
    -- printPuzzle output
    -- checkValue (puzzle !! 0)

    let t = modify_elem 0 2 'a' puzzle

    putStrLn("")

    printPuzzle t

