module SolverTest (tests) where

import Data.Array.IArray ((!))
import Data.List (sort)
import Helper (derivate)
import Solver (derivateValue, fill, fill', isSolved, isValid, place, solve)
import SudokuFixture
import Types (Sudoku(Board), Value)

tests :: [(String, Bool)]
tests =
  [ ("S-01 recognises a solved board", isSolved solved4)
  , ("S-02 rejects a board containing a zero as solved", not (isSolved singles4))
  , ("S-03 separates solvedness from validity", isSolved invalidFull4 && not (isValid invalidFull4))
  , ("S-04 accepts valid solved and partial boards", isValid solved4 && isValid singles4)
  , ("S-05 rejects duplicate values in rows, columns, and quadrants", all (not . isValid) [duplicateRow4, duplicateColumn4, duplicateQuadrant4])
  , ("S-06 rejects an empty square with no candidates", not (isValid noCandidate4))
  , ("S-07 accepts an empty board as valid", isValid empty4)
  , ("S-08 derivateValue ignores zero and singleton candidate cells", derivateValue mixedCandidates4 == ((4,1), [2,4]))
  , ("S-09 derivateValue chooses a shortest eligible candidate list", length (snd (derivateValue mixedCandidates4)) == 2)
  , ("S-10 derivateValue uses the solved-board sentinel", derivateValue solved4 == ((0,0), []))
  , ("S-11 derivateValue uses the sentinel without a branching position", derivateValue chainPass1 == ((0,0), []))
  , ("S-12 place updates the board and all availability dimensions", placementUpdatesAvailability)
  , ("S-13 place preserves unrelated state", place singles4 (1,2) 2 == placedSingles4)
  , ("S-14 a placed value is fixed and excluded from peers", placementDerivation)
  , ("S-15 fill' completes a one-pass naked-singles fixture", fill' singlePass4 == solved4)
  , ("S-16 fill' performs one propagation round only", fill' chain4 == chainPass1)
  , ("S-17 fill repeats propagation to a fixed point", fill chain4 == solved4)
  , ("S-18 fill' follows the recorded three-pass propagation chain", fill' threePassFill4 == threePass1 && fill' threePass1 == threePass2 && fill' threePass2 == solved4)
  , ("S-19 fill completes the three-pass fixture", fill threePassFill4 == solved4)
  , ("S-20 fill leaves empty and stalled boards unchanged", fill empty4 == empty4 && fill stalled4 == stalled4)
  , ("S-21 fill stops on a contradiction", not (isValid (fill noCandidate4)))
  , ("I-01 solve preserves an already solved puzzle", solve solved4 == [solved4])
  , ("I-02 solve completes deterministic singles", solve singles4 == [solved4])
  , ("I-03 solve incorporates repeated filling", solve chain4 == [solved4])
  , ("I-04 solve returns every branch solution exactly once", normalized (solve branch4) == normalized branchSolutions4 && hasNoDuplicates (map boardRows (solve branch4)))
  , ("I-05 solve rejects duplicate givens", solve duplicateRow4 == [])
  , ("I-06 solve rejects a no-candidate puzzle", solve noCandidate4 == [])
  , ("I-07 solve rejects a contradiction exposed after two search decisions", solve deepInvalid9 == [] && deepInvalidHasTwoLevelContradiction)
  , ("I-08 solve completes the unique 9x9 fixture", solve standardPuzzle9 == [standardSolution9])
  , ("I-09 returned solutions satisfy the raw completion oracle and preserve givens", solutionInvariant branch4 && solutionInvariant standardPuzzle9)
  ]

invalidFull4 :: Sudoku
invalidFull4 = sudokuFixture [[1,1,3,4], [3,4,1,2], [2,3,4,1], [4,2,2,3]]

placedSingles4 :: Sudoku
placedSingles4 = sudokuFixture [[1,2,0,4], [0,4,1,0], [2,0,4,0], [0,3,0,1]]

placementUpdatesAvailability :: Bool
placementUpdatesAvailability =
  case place singles4 (1,2) 2 of
    Board board linesAvailable columnsAvailable quadrantsAvailable _ _ ->
      board ! (1,2) == 2
      && not (linesAvailable ! (1,2))
      && not (columnsAvailable ! (2,2))
      && not (quadrantsAvailable ! (1,2))

placementDerivation :: Bool
placementDerivation =
  candidates placedSingles4 (1,2) == [2]
  && 2 `notElem` candidates placedSingles4 (1,3)
  && 2 `notElem` candidates placedSingles4 (2,2)
  && 2 `notElem` candidates placedSingles4 (2,1)

candidates :: Sudoku -> (Int, Int) -> [Value]
candidates sudoku@(Board _ _ _ _ dimension _) (line, column) =
  [fromIntegral value | value <- [1..dimension], derivate sudoku ! (line, column, value)]

normalized :: [Sudoku] -> [[[Int]]]
normalized = sort . map boardRows

solutionInvariant :: Sudoku -> Bool
solutionInvariant input =
  all (\solution -> isCompleteRaw solution && givensPreserved input solution) (solve input)

deepInvalidHasTwoLevelContradiction :: Bool
deepInvalidHasTwoLevelContradiction =
  isValid filled && not (isSolved filled)
  && rootPosition == (8,9) && rootValues == [4,9]
  && all firstPlacementHasInvalidSecondLevel rootValues
  where
    filled = fill deepInvalid9
    (rootPosition, rootValues) = derivateValue filled
    firstPlacementHasInvalidSecondLevel value =
      isValid firstBoard && not (isSolved firstBoard)
      && secondPosition == (7,9) && not (null secondValues)
      && all (not . isValid . fill . place firstBoard secondPosition) secondValues
      where
        firstBoard = fill (place filled rootPosition value)
        (secondPosition, secondValues) = derivateValue firstBoard
