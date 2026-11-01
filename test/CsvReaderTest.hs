module CsvReaderTest (tests) where

import Data.Array.Unboxed (UArray, array, (!))
import Data.List (transpose)
import Types (BoardPosition, Sudoku(Board), Value)
import CsvBoardReader (csvBoardReader)

tests :: [(String, Bool)]
tests =
  [ ("Sudoku vazio 1x1", boardMatches (csvRows empty1) empty1)
  , ("Sudoku vazio 4x4", boardMatches (csvRows empty4) empty4)
  , ("Sudoku vazio 9x9", boardMatches (csvRows empty9) empty9)
  , ("Sudoku completo 1x1", boardMatches (csvRows solved1) solved1)
  , ("Sudoku completo 4x4", boardMatches (csvRows solved4) solved4)
  , ("Sudoku completo 9x9", boardMatches (csvRows solved9) solved9)
  , ("Sudoku parcial 4x4", boardMatches (csvRows partial4) partial4)
  , ("Sudoku parcial 9x9", boardMatches (csvRows partial9) partial9)
  ]

boardMatches :: [String] -> Sudoku -> Bool
boardMatches rows fixture = csvBoardReader rows == Right fixture

csvRows :: Sudoku -> [String]
csvRows (Board board _ _ _ dimension _) =
  [join [show (fromIntegral (board ! (line, col)) :: Int) | col <- [1..dimension]]
  | line <- [1..dimension]]
  where
    join [] = ""
    join [field] = field
    join (field:fields) = field ++ "," ++ join fields

sudokuFixture :: [[Int]] -> Sudoku
sudokuFixture cells = Board board linesAvailable columnsAvailable quadrantsAvailable
                            dimension shortDimension
  where
    dimension = length cells
    shortDimension = findSquareRoot 1
    findSquareRoot candidate
      | candidate * candidate >= dimension = candidate
      | otherwise = findSquareRoot (candidate + 1)
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
    quadrantCells =
      [ (quadrant, concat
          [take shortDimension (drop colStart row)
          | row <- take shortDimension (drop rowStart cells)])
      | (quadrant, (rowStart, colStart)) <- zip [1..]
          [(row, col) | row <- [0, shortDimension .. dimension - 1]
                      , col <- [0, shortDimension .. dimension - 1]]]
    quadrantsAvailable = array ((1,1), (dimension,dimension))
      [((quadrant, value), value `notElem` values)
      | (quadrant, values) <- quadrantCells, value <- [1..dimension]]

empty1, empty4, empty9, solved1, solved4, solved9, partial4, partial9 :: Sudoku
empty1 = sudokuFixture [[0]]
empty4 = sudokuFixture (replicate 4 (replicate 4 0))
empty9 = sudokuFixture (replicate 9 (replicate 9 0))
solved1 = sudokuFixture [[1]]
solved4 = sudokuFixture
  [[1,2,3,4], [3,4,1,2], [2,1,4,3], [4,3,2,1]]
solved9 = sudokuFixture
  [ [1,2,3,4,5,6,7,8,9]
  , [4,5,6,7,8,9,1,2,3]
  , [7,8,9,1,2,3,4,5,6]
  , [2,3,4,5,6,7,8,9,1]
  , [5,6,7,8,9,1,2,3,4]
  , [8,9,1,2,3,4,5,6,7]
  , [3,4,5,6,7,8,9,1,2]
  , [6,7,8,9,1,2,3,4,5]
  , [9,1,2,3,4,5,6,7,8]
  ]
partial4 = sudokuFixture
  [[1,0,0,4], [0,4,1,0], [2,0,4,0], [0,3,0,1]]
partial9 = sudokuFixture
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
