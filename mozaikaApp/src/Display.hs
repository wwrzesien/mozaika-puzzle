module Display where
    
import System.IO (readFile)
import Util 
    ( toMatrix,
      Point(x, y, value_in, value_out) )

-- wczytaj łamigłówkę z pliku o podanej nazwie
readPuzzle :: String -> IO [String]
readPuzzle filename = do
    contents <- readFile filename -- odczytaj całą zawartość pliku
    let puzzle = read contents :: [String] -- utwórz listę napisów (zob. klasa typów Read)
    return puzzle

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

-- wyświetl rozwiązanie łamigłówki
displayResults :: Int -> [Point] -> IO ()
displayResults row_length table =
    let values_list = [ value_out point | point<-table]
        matrix = toMatrix row_length values_list
    in printPuzzle matrix


