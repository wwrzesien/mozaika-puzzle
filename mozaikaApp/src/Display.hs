module Display where
    
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