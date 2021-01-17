module Main where

import Display
import Solver

main :: IO ()
main = do 
    puzzle <- readPuzzle "puzzles/puzzle02.txt"
    putStrLn "Łamigłówka:"
    printPuzzle puzzle
    print puzzle

    let table = prepTable puzzle

    print table

    putStr "finish"



