import System.IO (readFile)

-- wczytaj łamigłówkę z pliku o podanej nazwie
readPuzzle :: String -> IO [String]
readPuzzle filename = do
    contents <- readFile filename -- odczytaj całą zawartość pliku
    let puzzle = read contents :: [String] -- utwórz listę napisów (zob. klasa typów Read)
    return puzzle

-- checkValue::String -> IO()
-- checkValue row = if elem '5' row then putStrLn("tak")
--     if elem '5'
--     else putStrLn("nie")

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

-- checkRow::String -> 

-- solver
-- solver::[String] -> [String]
-- solver [] = return ()
-- solver (x:xs) = do
--     checkRow x
--     checkRow xs

-- replace elem in nth position in row
replace::Int->a->[a]->[a]
replace _ _ [] = []
replace 0 a (_:xs) = a:xs
replace n a (x:xs) =
  if n < 0
    then (x:xs)
    else x: replace (n-1) a xs

-- modify_elem::Int->Int->Char->
modify_elem::Int->Int->Char->[String]->[String]
modify_elem row col a xs =
    let row_to_mod = xs !! row
        modified_row = replace col a row_to_mod
    in replace row modified_row xs


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



