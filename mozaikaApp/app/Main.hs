module Main where

import Display
import Solver

main :: IO ()
main = do 
    puzzle <- readPuzzle "puzzles/puzzle01.txt"
    putStrLn "Łamigłówka:"
    printPuzzle puzzle
    print puzzle

    let table = prepTable puzzle

    -- print table

    print(getElemValue 1 1 10 table)

    putStr "finish"



