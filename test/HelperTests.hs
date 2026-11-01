module HelperTests (tests) where

import Data.Array.IArray (bounds, (!))
import Helper (derivate)
import SudokuFixture
import Types (Sudoku(Board), Value)

tests :: [(String, Bool)]
tests =
  [ ("D-01 derives every value for an empty 4x4 board", allCandidates empty4 [1,2,3,4])
  , ("D-02 derives the fixed value for a given cell", candidates singles4 (1,1) == [1])
  , ("D-03 excludes values already used in a row", 1 `notElem` candidates singles4 (1,2))
  , ("D-04 excludes values already used in a column", 2 `notElem` candidates singles4 (2,1))
  , ("D-05 excludes values already used in a quadrant", 1 `notElem` candidates singles4 (2,1))
  , ("D-06 agrees with the independent raw-board candidate oracle", derivationMatchesOracle singles4)
  , ("D-07 represents a no-candidate cell without a fallback", candidates noCandidate4 (1,3) == [])
  , ("D-08 covers the complete 9x9 derivation bounds", bounds (derivate empty9) == ((1,1,1),(9,9,9)) && allCandidates empty9 [1..9])
  ]

candidates :: Sudoku -> (Int, Int) -> [Value]
candidates sudoku@(Board _ _ _ _ dimension _) (line, column) =
  [fromIntegral value | value <- [1..dimension], derivate sudoku ! (line, column, value)]

allCandidates :: Sudoku -> [Value] -> Bool
allCandidates sudoku@(Board _ _ _ _ dimension _) expected =
  and [candidates sudoku (line, column) == expected
      | line <- [1..dimension], column <- [1..dimension]]

derivationMatchesOracle :: Sudoku -> Bool
derivationMatchesOracle sudoku@(Board _ _ _ _ dimension _) =
  and [candidates sudoku (line, column) == candidateOracle sudoku (line, column)
      | line <- [1..dimension], column <- [1..dimension]]
