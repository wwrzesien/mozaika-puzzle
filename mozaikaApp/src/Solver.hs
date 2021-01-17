module Solver where

data Point = Point { x::Int, y::Int, value_in:: Char, value_out::Char} deriving (Show)


-- table = [Point {x = 0, y = 0, value = '.'},Point {x = 0, y = 1, value = '.'},Point {x = 0, y = 2, value = '5'},Point {x = 0, y = 3, value = '.'},Point {x = 0, y = 4, value = '.'},Point {x = 0, y = 5, value = '.'},Point {x = 0, y = 6, value = '.'},Point {x = 0, y = 7, value = '5'},Point {x = 0, y = 8, value = '4'},Point {x = 0, y = 9, value = '.'},Point {x = 1, y = 0, value = '.'},Point {x = 1, y = 1, value = '5'},Point {x = 1, y = 2, value = '.'},Point {x = 1, y = 3, value = '.'},Point {x = 1, y = 4, value = '6'},Point {x = 1, y = 5, value = '.'},Point {x = 1, y = 6, value = '.'},Point {x = 1, y = 7, value = '5'},Point {x = 1, y = 8, value = '.'},Point {x = 1, y = 9, value = '.'},Point {x = 2, y = 0, value = '4'},Point {x = 2, y = 1, value = '.'},Point {x = 2, y = 2, value = '2'},Point {x = 2, y = 3, value = '.'},Point {x = 2, y = 4, value = '5'},Point {x = 2, y = 5, value = '.'},Point {x = 2, y = 6, value = '.'},Point {x = 2, y = 7, value = '.'},Point {x = 2, y = 8, value = '4'},Point {x = 2, y = 9, value = '4'},Point {x = 3, y = 0, value = '.'},Point {x = 3, y = 1, value = '4'},Point {x = 3, y = 2, value = '.'},Point {x = 3, y = 3, value = '.'},Point {x = 3, y = 4, value = '.'},Point {x = 3, y = 5, value = '.'},Point {x = 3, y = 6, value = '1'},Point {x = 3, y = 7, value = '.'},Point {x = 3, y = 8, value = '.'},Point {x = 3, y = 9, value = '.'},Point {x = 4, y = 0, value = '.'},Point {x = 4, y = 1, value = '.'},Point {x = 4, y = 2, value = '.'},Point {x = 4, y = 3, value = '1'},Point {x = 4, y = 4, value = '.'},Point {x = 4, y = 5, value = '.'},Point {x = 4, y = 6, value = '1'},Point {x = 4, y = 7, value = '3'},Point {x = 4, y = 8, value = '.'},Point {x = 4, y = 9, value = '5'},Point {x = 5, y = 0, value = '.'},Point {x = 5, y = 1, value = '.'},Point {x = 5, y = 2, value = '.'},Point {x = 5, y = 3, value = '3'},Point {x = 5, y = 4, value = '.'},Point {x = 5, y = 5, value = '.'},Point {x = 5, y = 6, value = '3'},Point {x = 5, y = 7, value = '6'},Point {x = 5, y = 8, value = '.'},Point {x = 5, y = 9, value = '.'},Point {x = 6, y = 0, value = '.'},Point {x = 6, y = 1, value = '6'},Point {x = 6, y = 2, value = '7'},Point {x = 6, y = 3, value = '6'},Point {x = 6, y = 4, value = '.'},Point {x = 6, y = 5, value = '4'},Point {x = 6, y = 6, value = '.'},Point {x = 6, y = 7, value = '.'},Point {x = 6, y = 8, value = '.'},Point {x = 6, y = 9, value = '.'},Point {x = 7, y = 0, value = '.'},Point {x = 7, y = 1, value = '3'},Point {x = 7, y = 2, value = '.'},Point {x = 7, y = 3, value = '.'},Point {x = 7, y = 4, value = '7'},Point {x = 7, y = 5, value = '7'},Point {x = 7, y = 6, value = '.'},Point {x = 7, y = 7, value = '.'},Point {x = 7, y = 8, value = '3'},Point {x = 7, y = 9, value = '1'},Point {x = 8, y = 0, value = '.'},Point {x = 8, y = 1, value = '1'},Point {x = 8, y = 2, value = '3'},Point {x = 8, y = 3, value = '.'},Point {x = 8, y = 4, value = '8'},Point {x = 8, y = 5, value = '.'},Point {x = 8, y = 6, value = '.'},Point {x = 8, y = 7, value = '.'},Point {x = 8, y = 8, value = '1'},Point {x = 8, y = 9, value = '.'},Point {x = 9, y = 0, value = '.'},Point {x = 9, y = 1, value = '.'},Point {x = 9, y = 2, value = '.'},Point {x = 9, y = 3, value = '.'},Point {x = 9, y = 4, value = '.'},Point {x = 9, y = 5, value = '.'},Point {x = 9, y = 6, value = '3'},Point {x = 9, y = 7, value = '.'},Point {x = 9, y = 8, value = '.'},Point {x = 9, y = 9, value = '.'}]
table2 :: [Point]
table2 = [Point {x = 0, y = 0, value_in = '.', value_out = '.'},Point {x = 0, y = 1, value_in = '.', value_out = '.'},Point {x = 0, y = 2, value_in = '.', value_out = '.'},Point {x = 0, y = 3, value_in = '4', value_out = '4'},Point {x = 0, y = 4, value_in = '.', value_out = '.'},Point {x = 0, y = 5, value_in = '.', value_out = '.'},Point {x = 0, y = 6, value_in = '.', value_out = '.'},Point {x = 0, y = 7, value_in = '.', value_out = '.'},Point {x = 0, y = 8, value_in = '.', value_out = '.'},Point {x = 0, y = 9, value_in = '.', value_out = '.'},Point {x = 1, y = 0, value_in = '.', value_out = '.'},Point {x = 1, y = 1, value_in = '2', value_out = '2'},Point {x = 1, y = 2, value_in = '.', value_out = '.'},Point {x = 1, y = 3, value_in = '.', value_out = '.'},Point {x = 1, y = 4, value_in = '.', value_out = '.'},Point {x = 1, y = 5, value_in = '6', value_out = '6'},Point {x = 1, y = 6, value_in = '.', value_out = '.'},Point {x = 1, y = 7, value_in = '2', value_out = '2'},Point {x = 1, y = 8, value_in = '0', value_out = '0'},Point {x = 1, y = 9, value_in = '.', value_out = '.'},Point {x = 2, y = 0, value_in = '0', value_out = '0'},Point {x = 2, y = 1, value_in = '3', value_out = '3'},Point {x = 2, y = 2, value_in = '4', value_out = '4'},Point {x = 2, y = 3, value_in = '.', value_out = '.'},Point {x = 2, y = 4, value_in = '5', value_out = '5'},Point {x = 2, y = 5, value_in = '.', value_out = '.'},Point {x = 2, y = 6, value_in = '.', value_out = '.'},Point {x = 2, y = 7, value_in = '4', value_out = '4'},Point {x = 2, y = 8, value_in = '.', value_out = '.'},Point {x = 2, y = 9, value_in = '0', value_out = '0'},Point {x = 3, y = 0, value_in = '.', value_out = '.'},Point {x = 3, y = 1, value_in = '.', value_out = '.'},Point {x = 3, y = 2, value_in = '.', value_out = '.'},Point {x = 3, y = 3, value_in = '7', value_out = '7'},Point {x = 3, y = 4, value_in = '.', value_out = '.'},Point {x = 3, y = 5, value_in = '.', value_out = '.'},Point {x = 3, y = 6, value_in = '6', value_out = '6'},Point {x = 3, y = 7, value_in = '.', value_out = '.'},Point {x = 3, y = 8, value_in = '.', value_out = '.'},Point {x = 3, y = 9, value_in = '.', value_out = '.'},Point {x = 4, y = 0, value_in = '.', value_out = '.'},Point {x = 4, y = 1, value_in = '.', value_out = '.'},Point {x = 4, y = 2, value_in = '.', value_out = '.'},Point {x = 4, y = 3, value_in = '.', value_out = '.'},Point {x = 4, y = 4, value_in = '7', value_out = '7'},Point {x = 4, y = 5, value_in = '.', value_out = '.'},Point {x = 4, y = 6, value_in = '.', value_out = '.'},Point {x = 4, y = 7, value_in = '.', value_out = '.'},Point {x = 4, y = 8, value_in = '.', value_out = '.'},Point {x = 4, y = 9, value_in = '.', value_out = '.'},Point {x = 5, y = 0, value_in = '.', value_out = '.'},Point {x = 5, y = 1, value_in = '6', value_out = '6'},Point {x = 5, y = 2, value_in = '9', value_out = '9'},Point {x = 5, y = 3, value_in = '.', value_out = '.'},Point {x = 5, y = 4, value_in = '.', value_out = '.'},Point {x = 5, y = 5, value_in = '4', value_out = '4'},Point {x = 5, y = 6, value_in = '.', value_out = '.'},Point {x = 5, y = 7, value_in = '.', value_out = '.'},Point {x = 5, y = 8, value_in = '0', value_out = '0'},Point {x = 5, y = 9, value_in = '.', value_out = '.'},Point {x = 6, y = 0, value_in = '.', value_out = '.'},Point {x = 6, y = 1, value_in = '7', value_out = '7'},Point {x = 6, y = 2, value_in = '.', value_out = '.'},Point {x = 6, y = 3, value_in = '.', value_out = '.'},Point {x = 6, y = 4, value_in = '2', value_out = '2'},Point {x = 6, y = 5, value_in = '.', value_out = '.'},Point {x = 6, y = 6, value_in = '4', value_out = '4'},Point {x = 6, y = 7, value_in = '.', value_out = '.'},Point {x = 6, y = 8, value_in = '.', value_out = '.'},Point {x = 6, y = 9, value_in = '.', value_out = '.'},Point {x = 7, y = 0, value_in = '3', value_out = '3'},Point {x = 7, y = 1, value_in = '6', value_out = '6'},Point {x = 7, y = 2, value_in = '.', value_out = '.'},Point {x = 7, y = 3, value_in = '5', value_out = '5'},Point {x = 7, y = 4, value_in = '.', value_out = '.'},Point {x = 7, y = 5, value_in = '4', value_out = '4'},Point {x = 7, y = 6, value_in = '.', value_out = '.'},Point {x = 7, y = 7, value_in = '3', value_out = '3'},Point {x = 7, y = 8, value_in = '.', value_out = '.'},Point {x = 7, y = 9, value_in = '.', value_out = '.'},Point {x = 8, y = 0, value_in = '.', value_out = '.'},Point {x = 8, y = 1, value_in = '6', value_out = '6'},Point {x = 8, y = 2, value_in = '6', value_out = '6'},Point {x = 8, y = 3, value_in = '.', value_out = '.'},Point {x = 8, y = 4, value_in = '.', value_out = '.'},Point {x = 8, y = 5, value_in = '.', value_out = '.'},Point {x = 8, y = 6, value_in = '5', value_out = '5'},Point {x = 8, y = 7, value_in = '.', value_out = '.'},Point {x = 8, y = 8, value_in = '.', value_out = '.'},Point {x = 8, y = 9, value_in = '0', value_out = '0'},Point {x = 9, y = 0, value_in = '.', value_out = '.'},Point {x = 9, y = 1, value_in = '.', value_out = '.'},Point {x = 9, y = 2, value_in = '.', value_out = '.'},Point {x = 9, y = 3, value_in = '.', value_out = '.'},Point {x = 9, y = 4, value_in = '.', value_out = '.'},Point {x = 9, y = 5, value_in = '4', value_out = '4'},Point {x = 9, y = 6, value_in = '.', value_out = '.'},Point {x = 9, y = 7, value_in = '.', value_out = '.'},Point {x = 9, y = 8, value_in = '0', value_out = '0'},Point {x = 9, y = 9, value_in = '.', value_out = '.'}]


main :: IO ()
main = do
    putStrLn "Przed" 
    displayResults 10 table2
    putStrLn ""
    -- przypadek 0 9 4-rog 6-sciana
    let s = solveBasic 10 table2 table2
    putStrLn ""
    displayResults 10 s
    let s3 = solveDiff3 10 table2 s
    putStrLn "Wynik"
    displayResults 10 s3

    let res = solveRest 1 10 table2 s3
    
    putStrLn "Wynik"
    displayResults 10 res

stopCondition table table_result =
    let check = [ value_out a == value_out b | a<-table, b<-table_result]
    in and check

-- rozwiazanie prostych przypadkow 0 9 4-rog 6-sciana
solveBasic :: Int -> [Point] -> [Point] -> [Point]
solveBasic _ [] [] = []
solveBasic _ [a] out = out
solveBasic row_length (p:ps) out_table = 
    let neigh = returnElemNeigh p row_length out_table
        table
            | (value_in p == '0') = fillPoints neigh row_length 'n' out_table
            | (value_in p == '9') = fillPoints neigh row_length '+' out_table
            | (value_in p == '4' && length neigh == 4) = fillPoints neigh row_length '+' out_table
            | (value_in p == '6' && length neigh == 6) = fillPoints neigh row_length '+' out_table
            | otherwise = out_table
    in solveBasic row_length ps table

-- przypadek roznicy 3
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
        t = fillPoints (points_to_fill!!0) row_length 'n' out_table
    in fillPoints (points_to_fill!!1) row_length '+' t     

-- nie wychodzi z petli !!!
solveRest 0 row_length table out_table = out_table
solveRest run row_length table out_table = 
    let table_result = solverRest row_length table out_table
        run
            | stopCondition out_table table_result = 0 
            | otherwise = 1
    in solveRest run row_length table out_table

-- znajdz miejsca dla ktorych poprzednie zmiany uwtorzyły latwe przypadki
solverRest :: Int -> [Point] -> [Point] -> [Point]
solverRest _ [] [] = []
solverRest _ [a] out = out
solverRest row_length (p:ps) out_table =  
    let all_neigh = returnElemNeigh p row_length out_table
        plus_neigh = [ n | n<-all_neigh, value_out n == '+' ]
        rest_neigh = [ n | n<-all_neigh, value_out n /= '+' && value_out n /= 'n']

        table
            | (isNumber p && charToInt (value_in p) == length plus_neigh) = fillPoints rest_neigh row_length 'n' out_table
            | (isNumber p && charToInt (value_in p) - length plus_neigh == length rest_neigh) = fillPoints rest_neigh row_length '+' out_table
            | otherwise = out_table
    in solverRest row_length ps table

-- convert char to int
charToInt :: Char -> Int
charToInt char = read [char]::Int

-- spr czy punkt posiada cyfrę
isNumber :: Point -> Bool
isNumber point = value_in point /= '.' && value_in point /= 'n' && value_in point /= 'n'

-- przypisz pkt zadana wartosc
fillPoints :: [Point] -> Int -> Char -> [Point] -> [Point]
fillPoints [] _ _ table = table
fillPoints (x:xs) row_length value table = 
    let mod = modifyElemPoint x row_length value table
    in fillPoints xs row_length value mod

-- zwróć rożnicę wartości 2 pkt
valueDiff :: Point -> Point -> Int
valueDiff point point2 = charToInt (value_in point) - charToInt (value_in point2)

-- wyszukaj sasiadow zadanego pkt
returnElemNeigh :: Point -> Int -> [Point] -> [Point]
returnElemNeigh point row_length table= 
    let row = x point
        col = y point
        coord = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 0), (0, 1), (1, -1), (1, 0), (1, 1)]
        neighbours = [ getElem (row+x) (col+y) row_length table | (x, y)<-coord, elem (row+x) [0..(row_length-1)] && elem (col+y) [0..(row_length-1)] ]
    in neighbours

-- wyszukaj sasiadow zadanego pkt
returnElements :: Point -> [(Int, Int)] -> Int -> [a] -> [a]
returnElements point coord row_length table= 
    let row = x point
        col = y point
        neighbours = [ getElem (row+x) (col+y) row_length table | (x, y)<-coord, elem (row+x) [0..(row_length-1)] && elem (col+y) [0..(row_length-1)] ]
    in neighbours

-- spr czy pkt istnieje i spelnai warunki
isElemValid :: Int -> Int -> Int -> [Point] -> Bool
isElemValid x y row_length table = 
    elem x [0..(row_length-1)] && elem y [0..(row_length-1)] &&
        value_out (getElem x y row_length table) /= 'n' && value_out (getElem x y row_length table) /= '+'

-- przygotuj tablice punktow
prepTable :: [[Char]] ->[Point]
prepTable puzzle = 
    let coordinates = [(x,y) | x <-[0..(length(head puzzle) - 1)], y<- [0..(length(head puzzle) - 1)]]
        puzzle_list = concat puzzle
        table = createTable coordinates puzzle_list
    in table

-- utworz liste punktow
createTable::[(Int, Int)]->[Char]->[Point]
createTable [] [] = []
createTable (x:xs) (y:ys) = 
    let a =  fst x
        b = snd x
        point = Point a b y y
    in point : createTable xs ys

-- zamien n element w liscie
replace :: (Num t1, Ord t1) => t1 -> t2 -> [t2] -> [t2]
replace _ _ [] = []
replace 0 point (_:xs) = point:xs
replace n point (x:xs) =
  if n < 0
    then x:xs
    else x: replace (n-1) point xs

-- zmodyfikuj element
modifyElem :: Int -> Int -> Int -> Char -> [Point] -> [Point]
modifyElem row col row_length value table =
    let pos = getListPos row col row_length
        point = table !! pos
        modified_point = Point (x point) (y point) (value_in point) value
    in replace pos modified_point table

-- zmodyfikuj punkt
modifyElemPoint :: Point -> Int -> Char -> [Point] -> [Point]
modifyElemPoint point row_length value table =
    let pos = getListPos (x point) (y point) row_length
        modified_point = Point (x point) (y point) (value_in point) value
    in replace pos modified_point table

-- get elem value from spec position
getElem :: Int -> Int -> Int -> [a] -> a
getElem row col row_length table = 
    let pos = getListPos row col row_length
    in table !! pos

-- zwroc pozycje z liste na podstawie wspolrzednych
getListPos :: Num a => a -> a -> a -> a
getListPos row col row_length = row * row_length + col  


-- In display (move later)
--- oddziel komórki lamigłówki znakiem
insertSignInRow::String -> Char -> String
insertSignInRow [] a = []
insertSignInRow [x] a = [x]
insertSignInRow (x:xs) a = x : a : insertSignInRow xs a

-- wyświetl lamigłówkę
printPuzzle::[String] -> IO()
printPuzzle [] = return ()
printPuzzle (x:xs) = do
    let y = insertSignInRow x ' '
    putStrLn y
    printPuzzle xs

-- zamien liste na macierz
toMatrix :: Int -> [a] -> [[a]]
toMatrix _ [] = []
toMatrix row_length table = 
    let x = take row_length table
        xs = drop row_length table
    in x : toMatrix row_length xs

-- wyswietl puzzle
displayResults :: Int -> [Point] -> IO ()
displayResults row_length table =
    let values_list = [ value_out point | point<-table]
        matrix = toMatrix row_length values_list
    in printPuzzle matrix

