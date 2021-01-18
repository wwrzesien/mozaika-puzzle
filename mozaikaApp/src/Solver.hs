module Solver where

import Util
    ( charToInt,
      isNumber,
      valueDiff,
      fillPoints,
      returnElemNeigh,
      returnElements,
      Point(x, y, value_in, value_out) )

-- SOLVEBASIC
-- rozwiazanie prostych przypadkow 0, 9, 4-róg, 6-ściana
solveBasic :: Int -> [Point] -> [Point] -> [Point]
solveBasic _ [] [] = []
solveBasic _ [a] out = out
solveBasic row_length (p:ps) out_table = 
    let neigh = returnElemNeigh p row_length out_table
        table
            | (value_in p == '0') = fillPoints neigh row_length '_' out_table
            | (value_in p == '9') = fillPoints neigh row_length 'n' out_table
            | (value_in p == '4' && length neigh == 4) = fillPoints neigh row_length 'n' out_table
            | (value_in p == '6' && length neigh == 6) = fillPoints neigh row_length 'n' out_table
            | otherwise = out_table
    in solveBasic row_length ps table

-- SOLVEDIFF3
-- przypadek rożnicy 3 dla sąsiednich pkt
solveDiff3 :: Int -> [Point] -> [Point] -> [Point]
solveDiff3 _ [] [] = []
solveDiff3 _ [a] out = out
solveDiff3 row_length (p:ps) out_table = 
    let all_neigh = returnElemNeigh p row_length out_table
        pc = [ p2 | p2<-all_neigh, isNumber p2, elem (valueDiff p p2) [3, -3], x p == x p2 || y p == y p2 ]
        table = if isNumber p && not (null pc) then
            solverDiff3 row_length p (head pc) out_table
        else out_table
    in solveDiff3 row_length ps table

-- solver dla solveDiff3
solverDiff3 :: Int -> Point -> Point -> [Point] -> [Point]
solverDiff3 row_length p pc out_table =
    let diff = (x pc - x p, y pc - y p)
        coord
            | (diff == (0, -1)) = [[(-1, -2), (0, -2), (1, -2)], [(-1, 1), (0, 1), (1, 1)]]
            | (diff == (-1, 0)) = [[(-2, -1), (-2, 0), (-2, 1)], [(1, -1), (1, 0), (1, 1)]]
            | (diff == (0, 1)) = [[(-1, 2), (0, 2), (1, 2)], [(-1, -1), (0, -1), (1, -1)]]
            | (diff == (1, 0)) = [[(2, -1), (2, 0), (2, 1)], [(-1, -1), (-1, 0), (-1, 1)]]
        points_to_fill
            | (valueDiff pc p == -3) = [returnElements p (coord!!0) row_length out_table, returnElements p (coord!!1) row_length out_table]
            | (valueDiff pc p == 3) = [returnElements p (coord!!1) row_length out_table, returnElements p (coord!!0) row_length out_table]
        t = fillPoints (points_to_fill!!0) row_length '_' out_table
    in fillPoints (points_to_fill!!1) row_length 'n' t     

-- SOLVEREST
-- przeszukanie wszystkich pkt i wstawienie znaku tam gdzie można
solveRest :: (Eq t1, Eq t2, Num t2, Num t1) => t1 -> t2 -> Int -> [Point] -> [Point] -> [Point]
solveRest 0 iter row_length table out_table = out_table
solveRest _ 0 row_length table out_table = out_table
solveRest run iter row_length table out_table = 
    let table_result = solverRest row_length table out_table
        run
            | stopCondition out_table table_result = 0 
            | otherwise = 1
        n_iter = iter -1
    in solveRest run n_iter row_length table table_result

-- solver dla solveRest
solverRest :: Int -> [Point] -> [Point] -> [Point]
solverRest _ [] [] = []
solverRest _ [a] out = out
solverRest row_length (p:ps) out_table =  
    let all_neigh = returnElemNeigh p row_length out_table
        plus_neigh = [ n | n<-all_neigh, value_out n == 'n' ]
        rest_neigh = [ n | n<-all_neigh, value_out n /= 'n' && value_out n /= '_']
        table
            | (isNumber p && charToInt (value_in p) == length plus_neigh) = fillPoints rest_neigh row_length '_' out_table
            | (isNumber p && charToInt (value_in p) - length plus_neigh == length rest_neigh) = fillPoints rest_neigh row_length 'n' out_table
            | otherwise = out_table
    in solverRest row_length ps table

-- warunek stopu dla solverRest
stopCondition :: [Point] -> [Point] -> Bool
stopCondition table table_result =
    let check = [ value_out a == value_out b | (a, b)<-zip table table_result]
    in and check