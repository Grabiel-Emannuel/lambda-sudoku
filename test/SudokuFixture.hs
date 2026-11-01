module SudokuFixture where

import Data.Array.Unboxed (UArray, array, (!))
import Data.Int (Int8)
import Data.List (nub, sort, transpose)
import Types (BoardPosition, Sudoku(Board), Value)

sudokuFixture :: [[Int]] -> Sudoku
sudokuFixture cells = Board board linesAvailable columnsAvailable quadrantsAvailable
                            dimension shortDimension
  where
    dimension = length cells
    shortDimension = squareRoot 1
    squareRoot candidate
      | candidate * candidate == dimension = candidate
      | otherwise = squareRoot (candidate + 1)
    positions = [(line, col) | line <- [1..dimension], col <- [1..dimension]]
    board = array ((1,1), (dimension,dimension))
      [((line, col), fromIntegral (cells !! (line - 1) !! (col - 1)))
      | (line, col) <- positions] :: UArray BoardPosition Value
    linesAvailable = array ((1,1), (dimension,dimension))
      [((line, value), value `notElem` row)
      | (line, row) <- zip [1..] cells, value <- [1..dimension]]
    columnsAvailable = array ((1,1), (dimension,dimension))
      [((col, value), value `notElem` column)
      | (col, column) <- zip [1..] (transpose cells), value <- [1..dimension]]
    quadrantsAvailable = array ((1,1), (dimension,dimension))
      [((quadrant, value), value `notElem` values)
      | (quadrant, values) <- quadrantValues cells shortDimension
      , value <- [1..dimension]]

quadrantValues :: [[Int]] -> Int -> [(Int, [Int])]
quadrantValues cells side =
  [ (quadrant, concat
      [take side (drop columnStart row)
      | row <- take side (drop lineStart cells)])
  | (quadrant, (lineStart, columnStart)) <- zip [1..]
      [(line, column) | line <- [0, side .. dimension - 1]
                      , column <- [0, side .. dimension - 1]]]
  where
    dimension = length cells

boardRows :: Sudoku -> [[Int]]
boardRows (Board board _ _ _ dimension _) =
  [[fromIntegral (board ! (line, column)) | column <- [1..dimension]]
  | line <- [1..dimension]]

candidateOracle :: Sudoku -> (Int, Int) -> [Value]
candidateOracle sudoku@(Board board _ _ _ dimension side) (line, column)
  | board ! (line, column) /= 0 = [board ! (line, column)]
  | otherwise =
      [fromIntegral value | value <- [1..dimension], value `notElem` used]
  where
    used = rowValues ++ columnValues ++ quadrantValuesAt
    rowValues = [fromIntegral (board ! (line, col)) :: Int | col <- [1..dimension]]
    columnValues = [fromIntegral (board ! (row, column)) :: Int | row <- [1..dimension]]
    quadrant = ((line - 1) `div` side) * side + ((column - 1) `div` side)
    lineStart = (quadrant `div` side) * side + 1
    columnStart = (quadrant `mod` side) * side + 1
    quadrantValuesAt =
      [fromIntegral (board ! (row, col)) :: Int
      | row <- [lineStart..lineStart + side - 1]
      , col <- [columnStart..columnStart + side - 1]]

isCompleteRaw :: Sudoku -> Bool
isCompleteRaw sudoku@(Board _ _ _ _ dimension side) =
  all (== expected) (map sort (rows ++ columns ++ quadrants))
  where
    expected = [1..dimension]
    rows = boardRows sudoku
    columns = transpose rows
    quadrants = map snd (quadrantValues rows side)

givensPreserved :: Sudoku -> Sudoku -> Bool
givensPreserved (Board input _ _ _ dimension _) (Board output _ _ _ _ _) =
  and [input ! (line, column) == 0 || input ! (line, column) == output ! (line, column)
      | line <- [1..dimension], column <- [1..dimension]]

hasNoDuplicates :: Eq a => [a] -> Bool
hasNoDuplicates values = length values == length (nub values)

empty4, empty9, solved4, singles4, duplicateRow4, duplicateColumn4, duplicateQuadrant4 :: Sudoku
empty4 = sudokuFixture (replicate 4 (replicate 4 0))
empty9 = sudokuFixture (replicate 9 (replicate 9 0))
solved4 = sudokuFixture [[1,2,3,4], [3,4,1,2], [2,1,4,3], [4,3,2,1]]
singles4 = sudokuFixture [[1,0,0,4], [0,4,1,0], [2,0,4,0], [0,3,0,1]]
duplicateRow4 = sudokuFixture [[1,1,0,0], [0,0,0,0], [0,0,0,0], [0,0,0,0]]
duplicateColumn4 = sudokuFixture [[1,0,0,0], [1,0,0,0], [0,0,0,0], [0,0,0,0]]
duplicateQuadrant4 = sudokuFixture [[1,0,0,0], [0,1,0,0], [0,0,0,0], [0,0,0,0]]

noCandidate4, singlePass4, chain4, chainPass1, threePassFill4, threePass1, threePass2, stalled4, mixedCandidates4, branch4 :: Sudoku
noCandidate4 = sudokuFixture [[1,2,0,4], [0,4,3,0], [0,0,0,0], [0,0,0,0]]
singlePass4 = sudokuFixture [[0,2,3,4], [0,4,1,2], [2,1,4,3], [4,3,2,1]]
chain4 = sudokuFixture [[0,2,0,4], [0,4,1,2], [2,1,4,3], [4,3,2,1]]
chainPass1 = sudokuFixture [[0,2,3,4], [3,4,1,2], [2,1,4,3], [4,3,2,1]]
threePassFill4 = sudokuFixture [[0,0,0,0], [3,0,0,2], [0,1,0,3], [4,3,2,1]]
threePass1 = sudokuFixture [[0,0,0,4], [3,4,0,2], [2,1,4,3], [4,3,2,1]]
threePass2 = sudokuFixture [[1,2,0,4], [3,4,1,2], [2,1,4,3], [4,3,2,1]]
stalled4 = sudokuFixture [[1,2,0,0], [0,0,0,0], [0,0,0,0], [0,0,0,0]]
mixedCandidates4 = sudokuFixture [[3,4,0,4], [3,2,1,1], [0,3,3,0], [0,0,1,3]]
branch4 = sudokuFixture [[0,2,0,4], [0,4,0,2], [2,1,4,3], [4,3,2,1]]

branchSolutions4 :: [Sudoku]
branchSolutions4 =
  [ solved4
  , sudokuFixture [[3,2,1,4], [1,4,3,2], [2,1,4,3], [4,3,2,1]]
  ]

standardPuzzle9, standardSolution9 :: Sudoku
standardPuzzle9 = sudokuFixture
  [ [5,3,0,0,7,0,0,0,0]
  , [6,0,0,1,9,5,0,0,0]
  , [0,9,8,0,0,0,0,6,0]
  , [8,0,0,0,6,0,0,0,3]
  , [4,0,0,8,0,3,0,0,1]
  , [7,0,0,0,2,0,0,0,6]
  , [0,6,0,0,0,0,2,8,0]
  , [0,0,0,4,1,9,0,0,5]
  , [0,0,0,0,8,0,0,7,9]
  ]
standardSolution9 = sudokuFixture
  [ [5,3,4,6,7,8,9,1,2]
  , [6,7,2,1,9,5,3,4,8]
  , [1,9,8,3,4,2,5,6,7]
  , [8,5,9,7,6,1,4,2,3]
  , [4,2,6,8,5,3,7,9,1]
  , [7,1,3,9,2,4,8,5,6]
  , [9,6,1,5,3,7,2,8,4]
  , [2,8,7,4,1,9,6,3,5]
  , [3,4,5,2,8,6,1,7,9]
  ]

deepInvalid9 :: Sudoku
deepInvalid9 = sudokuFixture
  [ [0,0,0,0,7,0,0,1,2]
  , [0,0,2,0,0,0,3,0,0]
  , [0,0,0,0,0,0,0,0,0]
  , [0,0,0,0,0,0,0,0,3]
  , [0,0,0,8,0,0,7,0,0]
  , [0,0,0,9,0,0,0,0,0]
  , [0,0,0,0,0,0,0,8,0]
  , [0,0,0,0,1,0,6,5,0]
  , [0,0,0,0,0,6,0,7,0]
  ]
