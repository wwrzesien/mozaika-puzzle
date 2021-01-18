module Main where

import Display
import Solver ( solveBasic, solveDiff3, solveRest )
import Util

main :: IO ()
main = do 
    puzzle <- readPuzzle "puzzles/puzzle01.txt"
    putStrLn "Łamigłówka:"
    printPuzzle puzzle

    let row_length = length (head puzzle)
    let max_iter = 100

    -- zamien macierz na liste
    let table = prepTable puzzle

    -- rozwiazania
    let basic_cases = solveBasic row_length table table
    let diff_cases = solveDiff3 row_length table basic_cases
    let final_solver = solveRest 1 max_iter row_length table diff_cases

    putStrLn "Wynik"
    displayResults 10 final_solver


