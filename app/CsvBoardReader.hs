module CsvBoardReader (csvBoardReader) where

import Data.Array.Unboxed (UArray, array, (!))
import Data.Char (isSpace)
import Data.List (nub, transpose)
import Types (BoardPosition, Dimension, Sudoku(Board), Value)

-- | Parse CSV rows into a Sudoku. Zero (or an empty CSV field) denotes an
-- empty cell. The board must be square, have square sub-grids, and contain no
-- repeated non-zero value in any row, column, or sub-grid.
csvBoardReader :: [String] -> Either String Sudoku
csvBoardReader rows = do
  cells <- traverse parseRow rows
  let dimension = length cells
  if dimension == 0
    then Left "CSV board must contain at least one row"
    else pure ()
  if dimension > fromIntegral (maxBound :: Value)
    then Left "Board dimension exceeds the supported cell value range"
    else pure ()
  if any ((/= dimension) . length) cells
    then Left "CSV board must have the same number of columns as rows"
    else pure ()
  shortDimension <- maybe (Left "Board dimension must be a perfect square") Right
                    (squareRoot dimension)
  if any (any (\value -> value < 0 || value > dimension)) cells
    then Left "Cell values must be between 0 and the board dimension"
    else pure ()
  let entries = [((line, col), fromIntegral value :: Value)
                | (line, row) <- zip [1..] cells
                , (col, value) <- zip [1..] row]
  if hasDuplicates cells shortDimension
    then Left "Board contains duplicate values in a row, column, or sub-grid"
    else Right (makeSudoku dimension shortDimension entries)

parseRow :: String -> Either String [Int]
parseRow = traverse parseCell . splitCommas
  where
    parseCell field
      | null stripped = Right 0
      | otherwise = case reads stripped of
          [(value, "")] -> Right value
          _ -> Left ("Invalid CSV cell: " ++ show field)
      where
        stripped = dropWhileEndSpace (dropWhile isSpace field)

dropWhileEndSpace :: String -> String
dropWhileEndSpace = reverse . dropWhile isSpace . reverse

splitCommas :: String -> [String]
splitCommas [] = [""]
splitCommas input = case break (== ',') input of
  (field, []) -> [field]
  (field, _:rest) -> field : splitCommas rest

squareRoot :: Int -> Maybe Int
squareRoot n = seek 1
  where
    seek candidate
      | candidate * candidate == n = Just candidate
      | candidate * candidate > n = Nothing
      | otherwise = seek (candidate + 1)

hasDuplicates :: [[Int]] -> Dimension -> Bool
hasDuplicates cells shortDimension =
     any repeated cells
  || any repeated (transpose cells)
  || any repeated [ [cells !! (row - 1) !! (col - 1)
                    | row <- [rowStart .. rowStart + shortDimension - 1]
                    , col <- [colStart .. colStart + shortDimension - 1]]
                  | rowStart <- [1, 1 + shortDimension .. length cells]
                  , colStart <- [1, 1 + shortDimension .. length cells]]
  where
    repeated values = let givens = filter (/= 0) values
                      in length givens /= length (nub givens)

makeSudoku :: Dimension -> Dimension -> [(BoardPosition, Value)] -> Sudoku
makeSudoku dimension shortDimension entries =
  Board board linesAvailable columnsAvailable quadrantsAvailable
        dimension shortDimension
  where
    positions = [(line, col) | line <- [1..dimension], col <- [1..dimension]]
    board = array ((1,1), (dimension,dimension)) entries :: UArray BoardPosition Value
    linesAvailable = array ((1,1), (dimension,dimension))
      [((line, value), not (any (\col -> boardValue line col == fromIntegral value) [1..dimension]))
      | line <- [1..dimension], value <- [1..dimension]]
    columnsAvailable = array ((1,1), (dimension,dimension))
      [((col, value), not (any (\line -> boardValue line col == fromIntegral value) [1..dimension]))
      | col <- [1..dimension], value <- [1..dimension]]
    quadrantsAvailable = array ((1,1), (dimension,dimension))
      [((quadrant, value), not (any (\(line,col) -> boardValue line col == fromIntegral value)
          [position | position@(line,col) <- positions
                    , quadrantOf line col == quadrant]))
      | quadrant <- [1..dimension], value <- [1..dimension]]
    boardValue line col = board ! (line, col)
    quadrantOf line col =
      ((line - 1) `div` shortDimension) * shortDimension
        + ((col - 1) `div` shortDimension) + 1
