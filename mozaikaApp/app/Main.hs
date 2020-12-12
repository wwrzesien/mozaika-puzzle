import System.IO (readFile)
-- wczytaj łamigłówkę z pliku o podanej nazwie
readPuzzle :: String -> IO [String]
readPuzzle filename = do
    contents <- readFile filename -- odczytaj całą zawartość pliku
    let puzzle = read contents :: [String] -- utwórz listę napisów (zob. klasa typów Read)
    return puzzle

main = do 
    puzzle <- readPuzzle "puzzles/puzzle01.txt"
    putStrLn (show $ length $ puzzle) -- wyświetl liczbę wierszy łamigłówki
    putStrLn (show $ puzzle) -- wyświetl łamigłówkę