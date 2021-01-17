module Solver where

data Point = Point { x::Int, y::Int, value_in:: Char, value_out::Char} deriving (Show)


-- table = [Point {x = 0, y = 0, value = '.'},Point {x = 0, y = 1, value = '.'},Point {x = 0, y = 2, value = '5'},Point {x = 0, y = 3, value = '.'},Point {x = 0, y = 4, value = '.'},Point {x = 0, y = 5, value = '.'},Point {x = 0, y = 6, value = '.'},Point {x = 0, y = 7, value = '5'},Point {x = 0, y = 8, value = '4'},Point {x = 0, y = 9, value = '.'},Point {x = 1, y = 0, value = '.'},Point {x = 1, y = 1, value = '5'},Point {x = 1, y = 2, value = '.'},Point {x = 1, y = 3, value = '.'},Point {x = 1, y = 4, value = '6'},Point {x = 1, y = 5, value = '.'},Point {x = 1, y = 6, value = '.'},Point {x = 1, y = 7, value = '5'},Point {x = 1, y = 8, value = '.'},Point {x = 1, y = 9, value = '.'},Point {x = 2, y = 0, value = '4'},Point {x = 2, y = 1, value = '.'},Point {x = 2, y = 2, value = '2'},Point {x = 2, y = 3, value = '.'},Point {x = 2, y = 4, value = '5'},Point {x = 2, y = 5, value = '.'},Point {x = 2, y = 6, value = '.'},Point {x = 2, y = 7, value = '.'},Point {x = 2, y = 8, value = '4'},Point {x = 2, y = 9, value = '4'},Point {x = 3, y = 0, value = '.'},Point {x = 3, y = 1, value = '4'},Point {x = 3, y = 2, value = '.'},Point {x = 3, y = 3, value = '.'},Point {x = 3, y = 4, value = '.'},Point {x = 3, y = 5, value = '.'},Point {x = 3, y = 6, value = '1'},Point {x = 3, y = 7, value = '.'},Point {x = 3, y = 8, value = '.'},Point {x = 3, y = 9, value = '.'},Point {x = 4, y = 0, value = '.'},Point {x = 4, y = 1, value = '.'},Point {x = 4, y = 2, value = '.'},Point {x = 4, y = 3, value = '1'},Point {x = 4, y = 4, value = '.'},Point {x = 4, y = 5, value = '.'},Point {x = 4, y = 6, value = '1'},Point {x = 4, y = 7, value = '3'},Point {x = 4, y = 8, value = '.'},Point {x = 4, y = 9, value = '5'},Point {x = 5, y = 0, value = '.'},Point {x = 5, y = 1, value = '.'},Point {x = 5, y = 2, value = '.'},Point {x = 5, y = 3, value = '3'},Point {x = 5, y = 4, value = '.'},Point {x = 5, y = 5, value = '.'},Point {x = 5, y = 6, value = '3'},Point {x = 5, y = 7, value = '6'},Point {x = 5, y = 8, value = '.'},Point {x = 5, y = 9, value = '.'},Point {x = 6, y = 0, value = '.'},Point {x = 6, y = 1, value = '6'},Point {x = 6, y = 2, value = '7'},Point {x = 6, y = 3, value = '6'},Point {x = 6, y = 4, value = '.'},Point {x = 6, y = 5, value = '4'},Point {x = 6, y = 6, value = '.'},Point {x = 6, y = 7, value = '.'},Point {x = 6, y = 8, value = '.'},Point {x = 6, y = 9, value = '.'},Point {x = 7, y = 0, value = '.'},Point {x = 7, y = 1, value = '3'},Point {x = 7, y = 2, value = '.'},Point {x = 7, y = 3, value = '.'},Point {x = 7, y = 4, value = '7'},Point {x = 7, y = 5, value = '7'},Point {x = 7, y = 6, value = '.'},Point {x = 7, y = 7, value = '.'},Point {x = 7, y = 8, value = '3'},Point {x = 7, y = 9, value = '1'},Point {x = 8, y = 0, value = '.'},Point {x = 8, y = 1, value = '1'},Point {x = 8, y = 2, value = '3'},Point {x = 8, y = 3, value = '.'},Point {x = 8, y = 4, value = '8'},Point {x = 8, y = 5, value = '.'},Point {x = 8, y = 6, value = '.'},Point {x = 8, y = 7, value = '.'},Point {x = 8, y = 8, value = '1'},Point {x = 8, y = 9, value = '.'},Point {x = 9, y = 0, value = '.'},Point {x = 9, y = 1, value = '.'},Point {x = 9, y = 2, value = '.'},Point {x = 9, y = 3, value = '.'},Point {x = 9, y = 4, value = '.'},Point {x = 9, y = 5, value = '.'},Point {x = 9, y = 6, value = '3'},Point {x = 9, y = 7, value = '.'},Point {x = 9, y = 8, value = '.'},Point {x = 9, y = 9, value = '.'}]
table2 :: [Point]
table2 = [Point {x = 0, y = 0, value_in = '.', value_out = '.'},Point {x = 0, y = 1, value_in = '.', value_out = '.'},Point {x = 0, y = 2, value_in = '.', value_out = '.'},Point {x = 0, y = 3, value_in = '4', value_out = '4'},Point {x = 0, y = 4, value_in = '.', value_out = '.'},Point {x = 0, y = 5, value_in = '.', value_out = '.'},Point {x = 0, y = 6, value_in = '.', value_out = '.'},Point {x = 0, y = 7, value_in = '.', value_out = '.'},Point {x = 0, y = 8, value_in = '.', value_out = '.'},Point {x = 0, y = 9, value_in = '.', value_out = '.'},Point {x = 1, y = 0, value_in = '.', value_out = '.'},Point {x = 1, y = 1, value_in = '2', value_out = '2'},Point {x = 1, y = 2, value_in = '.', value_out = '.'},Point {x = 1, y = 3, value_in = '.', value_out = '.'},Point {x = 1, y = 4, value_in = '.', value_out = '.'},Point {x = 1, y = 5, value_in = '6', value_out = '6'},Point {x = 1, y = 6, value_in = '.', value_out = '.'},Point {x = 1, y = 7, value_in = '2', value_out = '2'},Point {x = 1, y = 8, value_in = '0', value_out = '0'},Point {x = 1, y = 9, value_in = '.', value_out = '.'},Point {x = 2, y = 0, value_in = '0', value_out = '0'},Point {x = 2, y = 1, value_in = '3', value_out = '3'},Point {x = 2, y = 2, value_in = '4', value_out = '4'},Point {x = 2, y = 3, value_in = '.', value_out = '.'},Point {x = 2, y = 4, value_in = '5', value_out = '5'},Point {x = 2, y = 5, value_in = '.', value_out = '.'},Point {x = 2, y = 6, value_in = '.', value_out = '.'},Point {x = 2, y = 7, value_in = '4', value_out = '4'},Point {x = 2, y = 8, value_in = '.', value_out = '.'},Point {x = 2, y = 9, value_in = '0', value_out = '0'},Point {x = 3, y = 0, value_in = '.', value_out = '.'},Point {x = 3, y = 1, value_in = '.', value_out = '.'},Point {x = 3, y = 2, value_in = '.', value_out = '.'},Point {x = 3, y = 3, value_in = '7', value_out = '7'},Point {x = 3, y = 4, value_in = '.', value_out = '.'},Point {x = 3, y = 5, value_in = '.', value_out = '.'},Point {x = 3, y = 6, value_in = '6', value_out = '6'},Point {x = 3, y = 7, value_in = '.', value_out = '.'},Point {x = 3, y = 8, value_in = '.', value_out = '.'},Point {x = 3, y = 9, value_in = '.', value_out = '.'},Point {x = 4, y = 0, value_in = '.', value_out = '.'},Point {x = 4, y = 1, value_in = '.', value_out = '.'},Point {x = 4, y = 2, value_in = '.', value_out = '.'},Point {x = 4, y = 3, value_in = '.', value_out = '.'},Point {x = 4, y = 4, value_in = '7', value_out = '7'},Point {x = 4, y = 5, value_in = '.', value_out = '.'},Point {x = 4, y = 6, value_in = '.', value_out = '.'},Point {x = 4, y = 7, value_in = '.', value_out = '.'},Point {x = 4, y = 8, value_in = '.', value_out = '.'},Point {x = 4, y = 9, value_in = '.', value_out = '.'},Point {x = 5, y = 0, value_in = '.', value_out = '.'},Point {x = 5, y = 1, value_in = '6', value_out = '6'},Point {x = 5, y = 2, value_in = '9', value_out = '9'},Point {x = 5, y = 3, value_in = '.', value_out = '.'},Point {x = 5, y = 4, value_in = '.', value_out = '.'},Point {x = 5, y = 5, value_in = '4', value_out = '4'},Point {x = 5, y = 6, value_in = '.', value_out = '.'},Point {x = 5, y = 7, value_in = '.', value_out = '.'},Point {x = 5, y = 8, value_in = '0', value_out = '0'},Point {x = 5, y = 9, value_in = '.', value_out = '.'},Point {x = 6, y = 0, value_in = '.', value_out = '.'},Point {x = 6, y = 1, value_in = '7', value_out = '7'},Point {x = 6, y = 2, value_in = '.', value_out = '.'},Point {x = 6, y = 3, value_in = '.', value_out = '.'},Point {x = 6, y = 4, value_in = '2', value_out = '2'},Point {x = 6, y = 5, value_in = '.', value_out = '.'},Point {x = 6, y = 6, value_in = '4', value_out = '4'},Point {x = 6, y = 7, value_in = '.', value_out = '.'},Point {x = 6, y = 8, value_in = '.', value_out = '.'},Point {x = 6, y = 9, value_in = '.', value_out = '.'},Point {x = 7, y = 0, value_in = '3', value_out = '3'},Point {x = 7, y = 1, value_in = '6', value_out = '6'},Point {x = 7, y = 2, value_in = '.', value_out = '.'},Point {x = 7, y = 3, value_in = '5', value_out = '5'},Point {x = 7, y = 4, value_in = '.', value_out = '.'},Point {x = 7, y = 5, value_in = '4', value_out = '4'},Point {x = 7, y = 6, value_in = '.', value_out = '.'},Point {x = 7, y = 7, value_in = '3', value_out = '3'},Point {x = 7, y = 8, value_in = '.', value_out = '.'},Point {x = 7, y = 9, value_in = '.', value_out = '.'},Point {x = 8, y = 0, value_in = '.', value_out = '.'},Point {x = 8, y = 1, value_in = '6', value_out = '6'},Point {x = 8, y = 2, value_in = '6', value_out = '6'},Point {x = 8, y = 3, value_in = '.', value_out = '.'},Point {x = 8, y = 4, value_in = '.', value_out = '.'},Point {x = 8, y = 5, value_in = '.', value_out = '.'},Point {x = 8, y = 6, value_in = '5', value_out = '5'},Point {x = 8, y = 7, value_in = '.', value_out = '.'},Point {x = 8, y = 8, value_in = '.', value_out = '.'},Point {x = 8, y = 9, value_in = '0', value_out = '0'},Point {x = 9, y = 0, value_in = '.', value_out = '.'},Point {x = 9, y = 1, value_in = '.', value_out = '.'},Point {x = 9, y = 2, value_in = '.', value_out = '.'},Point {x = 9, y = 3, value_in = '.', value_out = '.'},Point {x = 9, y = 4, value_in = '.', value_out = '.'},Point {x = 9, y = 5, value_in = '4', value_out = '4'},Point {x = 9, y = 6, value_in = '.', value_out = '.'},Point {x = 9, y = 7, value_in = '.', value_out = '.'},Point {x = 9, y = 8, value_in = '0', value_out = '0'},Point {x = 9, y = 9, value_in = '.', value_out = '.'}]


-- rozwiaz
    -- let table = if value_in p == '0' then 
    --         if (returnElemNeigh p row_length out_table)
    --         case09 (x p) (y p) row_length '_' out_table 
    --     else 
    --         if value_in p == '9' then 
    --             case09 (x p) (y p) row_length '+' out_table 
    --         else out_table
    -- let neigh = returnElemNeigh p row_length out_table
    --     table = case (value_in p) of
    --         '0' -> if (length (neigh) == 9) then 
    --             fillPoints neigh row_length '+' out_table
    -- in solve row_length ps table

main :: IO ()
main = do
    let s = solve 10 table2 table2
    displayResults 10 table2
    putStrLn ""
    displayResults 10 s

-- rozwiazanie prostych przypadkow
solve :: Int -> [Point] -> [Point] -> [Point]
solve _ [] [] = []
solve _ [a] out = out
solve row_length (p:ps) out_table = 
    let neigh = returnElemNeigh p row_length out_table
        table
            | (value_in p == '0') = fillPoints neigh row_length 'n' out_table
            | (value_in p == '9') = fillPoints neigh row_length '+' out_table
            | (value_in p == '4' && length neigh == 4) = fillPoints neigh row_length '+' out_table
            | (value_in p == '6' && length neigh == 6) = fillPoints neigh row_length '+' out_table
            | otherwise = out_table
    in solve row_length ps table

-- 
fillPoints :: [Point] -> Int -> Char -> [Point] -> [Point]
fillPoints [] _ _ table = table
fillPoints (x:xs) row_length value table = 
    let mod = modifyElemPoint x row_length value table
    in fillPoints xs row_length value mod

-- wyszukaj sasiadow zadanego pkt
returnElemNeigh :: Point -> Int -> [Point] -> [Point]
returnElemNeigh point row_length table= 
    let row = x point
        col = y point
        coord = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 0), (0, 1), (1, -1), (1, 0), (1, 1)]
        neighbours = [ getElem (row+x) (col+y) row_length table | (x, y)<-coord, isElemValid (row+x) (col+y) row_length table ]
    in neighbours

-- spr czy pkt istnieje i spelnai warunki
isElemValid :: Int -> Int -> Int -> [Point] -> Bool
isElemValid x y row_length table = 
    elem x [0..(row_length-1)] && elem y [0..(row_length-1)] &&
        value_out (getElem x y row_length table) /= 'n'

-- przygotuj tablice punktow
prepTable :: [[Char]] ->[Point]
prepTable puzzle = 
    let coordinates = [(x,y) | x <-[0..(length(head puzzle) - 1)], y<- [0..(length(head puzzle) - 1)]]
        puzzle_list = toList puzzle
        table = createTable coordinates puzzle_list
    in table

-- zamien matrix na liste
toList::[[a]]->[a]
toList [] = []
toList (x:xs) = x ++ (toList xs)

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

