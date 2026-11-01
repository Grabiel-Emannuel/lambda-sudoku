module IOHelper (ioHelper) where

import CsvBoardReader (csvBoardReader)
import Types (Sudoku)

-- | Read a CSV file and parse its contents as a Sudoku.
ioHelper :: String -> IO (Either String Sudoku)
ioHelper path = do
  contents <- readFile path
  pure (csvBoardReader (lines contents))
