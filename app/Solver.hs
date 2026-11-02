module Solver where

import Data.Sort (sortOn)
import Data.Array.Unboxed ((!), (//))
import Types (BoardPosition, DerivateValue, Sudoku(Board), Value)
import Helper (derivate, isEmpty, isValidDerivate, whichQuadrant)

solve :: Sudoku -> [Sudoku]
solve board
  | (not . isValid) board       = []  
  | isSolved board              = [board]
  | (not . isValid) boardFilled = []
  | isSolved boardFilled        = [boardFilled]
  | (not . isPosValid) pos      = [] 
  | otherwise                   = solveds
  where
    boardFilled  = fill board
    pos@(position, values) = derivateValue boardFilled
    solveds = (concat . map (solve. place boardFilled position)) values

isPosValid :: DerivateValue -> Bool
isPosValid ((0,0), []) = False
isPosValid _ = True

fill :: Sudoku -> Sudoku
fill sudoku
  | (not . isValid) sudoku' = sudoku'
  | sudoku == sudoku' = sudoku'
  | otherwise = fill sudoku'
  where
    sudoku' = fill' sudoku

fill' :: Sudoku -> Sudoku
fill' sudoku@(Board board lins cols quads dimension shortDimension) = 
  Board (board // changeBoard) (lins // changeLines) (cols // changeCols) (quads // changeQuads) dimension shortDimension
  where
    derivated = derivate sudoku
    changePos = filter (null . (drop 1) . snd) 
                  [((i,j), [fromIntegral k | k <- [1..dimension], derivated ! (i,j,k)]) 
                    | i <- [1..dimension], j <- [1..dimension], 
                      (isEmpty . fromIntegral) (board ! (i,j)) ] 
    changeLines = [((line, fromIntegral value), False) | ((line, _), [value]) <- changePos]
    changeCols  = [((col,  fromIntegral value),  False) | ((_, col),  [value]) <- changePos]
    changeQuads = [((whichQuadrant (line, col) shortDimension, fromIntegral value), False) | ((line, col), [value]) <- changePos]
    changeBoard = [((line, col), value) | ((line, col), [value]) <- changePos]

isSolved :: Sudoku -> Bool
isSolved (Board board _ _ _ dimension _) =
  (null . filter isEmpty) 
    [board ! (i, j) 
      | i <- [1..dimension], j <- [1..dimension]]

isValid :: Sudoku -> Bool
isValid sudoku = 
  (isValidDerivate sudoku . derivate) sudoku

derivateValue :: Sudoku -> DerivateValue
derivateValue sudoku@(Board board _ _ _ dimension _) =
  (head' . sortOn (length . snd) . filter (not . null . (drop 1) . snd)) derivatedAsPositions
  where
    derivated = derivate sudoku
    derivatedAsPositions = [((i,j), [fromIntegral k | k <- [1..dimension], derivated ! (i,j,k)]) 
                             | i <- [1..dimension], j <- [1..dimension], 
                               (isEmpty . fromIntegral) (board ! (i,j)) ]
    head' :: [DerivateValue] -> DerivateValue
    head' [] = ((0,0), [])
    head' (n:_) = n

place :: Sudoku -> BoardPosition -> Value -> Sudoku
place (Board board lins cols quads dimension shortDimension) (line, col) value =
  Board (board // [((line, col), value)]) 
        (lins  // [((line, fromIntegral value), False)]) 
        (cols  // [((col,  fromIntegral value), False)]) 
        (quads // [((whichQuadrant (line, col) shortDimension,  fromIntegral value), False)])
        dimension shortDimension
