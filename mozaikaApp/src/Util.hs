module Util where

-- TYP PUNKT
data Point = Point { x::Int, y::Int, value_in:: Char, value_out::Char} deriving (Show)

-- OPERACJA NA WARTOŚCIACH PKT
-- zamień char na int
charToInt :: Char -> Int
charToInt char = read [char]::Int

-- spr czy punkt posiada cyfrę
isNumber :: Point -> Bool
isNumber point = value_in point /= '.' && value_in point /= '_' && value_in point /= '_'

-- zwróć rożnicę wartości 2 pkt
valueDiff :: Point -> Point -> Int
valueDiff point point2 = charToInt (value_in point) - charToInt (value_in point2)


-- MODYFIKACJA WARTOŚCI PKT
-- przypisz pkt zadaną wartość
fillPoints :: [Point] -> Int -> Char -> [Point] -> [Point]
fillPoints [] _ _ table = table
fillPoints (x:xs) row_length value table = 
    let mod = modifyElemPoint x row_length value table
    in fillPoints xs row_length value mod

-- zmodyfikuj punkt
modifyElemPoint :: Point -> Int -> Char -> [Point] -> [Point]
modifyElemPoint point row_length value table =
    let pos = getListPos (x point) (y point) row_length
        modified_point = Point (x point) (y point) (value_in point) value
    in replace pos modified_point table

-- zamień n-ty element w liście
replace :: (Num t1, Ord t1) => t1 -> t2 -> [t2] -> [t2]
replace _ _ [] = []
replace 0 point (_:xs) = point:xs
replace n point (x:xs) =
  if n < 0
    then x:xs
    else x: replace (n-1) point xs


-- FUNKCJE ZWRACAJĄCE PKT
-- wyszukaj wszystkich sasiadów zadanego pkt
returnElemNeigh :: Point -> Int -> [Point] -> [Point]
returnElemNeigh point row_length table= 
    let row = x point
        col = y point
        coord = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 0), (0, 1), (1, -1), (1, 0), (1, 1)]
        neighbours = [ getElem (row+x) (col+y) row_length table | (x, y)<-coord, elem (row+x) [0..(row_length-1)] && elem (col+y) [0..(row_length-1)] ]
    in neighbours

-- wyszukaj sasiadów zadanego pkt o określonych współrzędnych
returnElements :: Point -> [(Int, Int)] -> Int -> [a] -> [a]
returnElements point coord row_length table= 
    let row = x point
        col = y point
        neighbours = [ getElem (row+x) (col+y) row_length table | (x, y)<-coord, elem (row+x) [0..(row_length-1)] && elem (col+y) [0..(row_length-1)] ]
    in neighbours

-- zwróć pkt o zadanych wpsółrzędnych
getElem :: Int -> Int -> Int -> [a] -> a
getElem row col row_length table = 
    let pos = getListPos row col row_length
    in table !! pos

-- zwroc pozycję z liste na podstawie współrzędnych
getListPos :: Num a => a -> a -> a -> a
getListPos row col row_length = row * row_length + col  


-- FUNKCJE PRZYGOTUJĄCE DANE
-- przygotuj tablicę punktów
prepTable :: [[Char]] ->[Point]
prepTable puzzle = 
    let coordinates = [(x,y) | x <-[0..(length(head puzzle) - 1)], y<- [0..(length(head puzzle) - 1)]]
        puzzle_list = concat puzzle
        table = createTable coordinates puzzle_list
    in table

-- utwórz listę punktów
createTable::[(Int, Int)]->[Char]->[Point]
createTable [] [] = []
createTable (x:xs) (y:ys) = 
    let a =  fst x
        b = snd x
        point = Point a b y y
    in point : createTable xs ys

-- zamień listę na macierz
toMatrix :: Int -> [a] -> [[a]]
toMatrix _ [] = []
toMatrix row_length table = 
    let x = take row_length table
        xs = drop row_length table
    in x : toMatrix row_length xs