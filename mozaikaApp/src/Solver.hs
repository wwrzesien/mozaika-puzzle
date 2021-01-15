module Solver where

data Point = Point { x::Int, y::Int, value:: Char} deriving (Show)

table :: [Point]
table = [Point {x = 0, y = 0, value = '.'},Point {x = 0, y = 1, value = '.'},Point {x = 0, y = 2, value = '5'},Point {x = 0, y = 3, value = '.'},Point {x = 0, y = 4, value = '.'},Point {x = 0, y = 5, value = '.'},Point {x = 0, y = 6, value = '.'},Point {x = 0, y = 7, value = '5'},Point {x = 0, y = 8, value = '4'},Point {x = 0, y = 9, value = '.'},Point {x = 1, y = 0, value = '.'},Point {x = 1, y = 1, value = '5'},Point {x = 1, y = 2, value = '.'},Point {x = 1, y = 3, value = '.'},Point {x = 1, y = 4, value = '6'},Point {x = 1, y = 5, value = '.'},Point {x = 1, y = 6, value = '.'},Point {x = 1, y = 7, value = '5'},Point {x = 1, y = 8, value = '.'},Point {x = 1, y = 9, value = '.'},Point {x = 2, y = 0, value = '4'},Point {x = 2, y = 1, value = '.'},Point {x = 2, y = 2, value = '2'},Point {x = 2, y = 3, value = '.'},Point {x = 2, y = 4, value = '5'},Point {x = 2, y = 5, value = '.'},Point {x = 2, y = 6, value = '.'},Point {x = 2, y = 7, value = '.'},Point {x = 2, y = 8, value = '4'},Point {x = 2, y = 9, value = '4'},Point {x = 3, y = 0, value = '.'},Point {x = 3, y = 1, value = '4'},Point {x = 3, y = 2, value = '.'},Point {x = 3, y = 3, value = '.'},Point {x = 3, y = 4, value = '.'},Point {x = 3, y = 5, value = '.'},Point {x = 3, y = 6, value = '1'},Point {x = 3, y = 7, value = '.'},Point {x = 3, y = 8, value = '.'},Point {x = 3, y = 9, value = '.'},Point {x = 4, y = 0, value = '.'},Point {x = 4, y = 1, value = '.'},Point {x = 4, y = 2, value = '.'},Point {x = 4, y = 3, value = '1'},Point {x = 4, y = 4, value = '.'},Point {x = 4, y = 5, value = '.'},Point {x = 4, y = 6, value = '1'},Point {x = 4, y = 7, value = '3'},Point {x = 4, y = 8, value = '.'},Point {x = 4, y = 9, value = '5'},Point {x = 5, y = 0, value = '.'},Point {x = 5, y = 1, value = '.'},Point {x = 5, y = 2, value = '.'},Point {x = 5, y = 3, value = '3'},Point {x = 5, y = 4, value = '.'},Point {x = 5, y = 5, value = '.'},Point {x = 5, y = 6, value = '3'},Point {x = 5, y = 7, value = '6'},Point {x = 5, y = 8, value = '.'},Point {x = 5, y = 9, value = '.'},Point {x = 6, y = 0, value = '.'},Point {x = 6, y = 1, value = '6'},Point {x = 6, y = 2, value = '7'},Point {x = 6, y = 3, value = '6'},Point {x = 6, y = 4, value = '.'},Point {x = 6, y = 5, value = '4'},Point {x = 6, y = 6, value = '.'},Point {x = 6, y = 7, value = '.'},Point {x = 6, y = 8, value = '.'},Point {x = 6, y = 9, value = '.'},Point {x = 7, y = 0, value = '.'},Point {x = 7, y = 1, value = '3'},Point {x = 7, y = 2, value = '.'},Point {x = 7, y = 3, value = '.'},Point {x = 7, y = 4, value = '7'},Point {x = 7, y = 5, value = '7'},Point {x = 7, y = 6, value = '.'},Point {x = 7, y = 7, value = '.'},Point {x = 7, y = 8, value = '3'},Point {x = 7, y = 9, value = '1'},Point {x = 8, y = 0, value = '.'},Point {x = 8, y = 1, value = '1'},Point {x = 8, y = 2, value = '3'},Point {x = 8, y = 3, value = '.'},Point {x = 8, y = 4, value = '8'},Point {x = 8, y = 5, value = '.'},Point {x = 8, y = 6, value = '.'},Point {x = 8, y = 7, value = '.'},Point {x = 8, y = 8, value = '1'},Point {x = 8, y = 9, value = '.'},Point {x = 9, y = 0, value = '.'},Point {x = 9, y = 1, value = '.'},Point {x = 9, y = 2, value = '.'},Point {x = 9, y = 3, value = '.'},Point {x = 9, y = 4, value = '.'},Point {x = 9, y = 5, value = '.'},Point {x = 9, y = 6, value = '3'},Point {x = 9, y = 7, value = '.'},Point {x = 9, y = 8, value = '.'},Point {x = 9, y = 9, value = '.'}]


-- parseRow::(Int->Char)->Int->[Point]
-- parseRow (a b) c = 

-- parseData::String->[Point]
-- parseData [] = return ()
-- parseData (x:xs) = do
--     let a = Point {x=0, y=0, value=x}
--     a : xs
--     init xs
--     parseData xs

-- parseRow::String->[Point]->[Point]
-- parseRow [] [] = []
-- parseRow (x:xs) a = do
--     let b = Point 0 0 x
--     let c = b:a
--     parseRow xs c

-- b = parseData "qwe"

-- createPoint::Int

-- basic cases:
--  0 
--  9
-- basicCases [] [] = []
-- basicCases (x:xs) a = 
--     | x==0 = []
--     | otherwise = basicCases xs

-- checkRow [] [] = return ()
-- checkRow (x:xs) a =
--     if x == '0' 
--         then checkRow xs (modify_elem ) 
--         else if x == '9' 
--             then putStrLn("9")
--             else putStrLn("else")
     

-- checkValue::String -> IO()
-- checkValue row = if elem '5' row then putStrLn("tak")
--     if elem '5'
--     else putStrLn("nie")

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
        point = Point a b y
    in point : createTable xs ys

-- replace elem in nth position in row
-- replace _ _ _ [] = []
-- replace 0 0 Point x y a (_:xs) = a:xs
-- replace n a (x:xs) =
--   if n < 0
--     then x:xs
--     else x: replace (n-1) a xs

-- replace elem in spec position in 2d array
-- modifyElem::Int->Int->Char->[String]->[String]
-- modifyElem row col a xs =
--     let row_to_mod = xs !! row
--         modified_row = replace col a row_to_mod
--     in replace row modified_row xs

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
        modified_point = Point (x point) (y point) value
    in replace pos modified_point table

-- get elem value from spec position
-- getElemValue :: Int -> Int -> Int -> [a] -> a
-- getElemValue row col row_length table = 
--     let pos = getListPos row col row_length
--     in table !! pos

-- zwroc pozycje z liste na podstawie wspolrzednych
getListPos :: Num a => a -> a -> a -> a
getListPos row col row_length = row * row_length + col  
