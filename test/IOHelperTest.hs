module IOHelperTest (tests) where

import CsvReaderTest (partial4)
import IOHelper (ioHelper)

tests :: IO [(String, Bool)]
tests = do
  validBoard <- ioHelper "test/fixtures/partial4.csv"
  invalidBoard <- ioHelper "test/fixtures/invalid4.csv"
  pure
    [ ("IOHelper reads a partially filled Sudoku from CSV", validBoard == Right partial4)
    , ("IOHelper propagates invalid CSV errors", case invalidBoard of
        Left _ -> True
        Right _ -> False)
    ]
