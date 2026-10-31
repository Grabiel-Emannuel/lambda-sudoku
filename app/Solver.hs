module Solver where

import Data.Array.ST ((!))

import Types (Sudoku(Board), Position, Value)

import Helper (isEmpty, isValue, whichQuadrant)

solve :: Sudoku -> [Sudoku]
solve board
  | (not . isValid) board = []  
  | isSolved board        = [board]
  | isSolved boardFilled  = [boardFilled]
  | otherwise             = solveds
  where
    boardFilled        = fill board
    (position, values) = derivate boardFilled
    solveds            = (concatMap solve . map (place boardFilled position)) values

fill :: Sudoku -> Sudoku
fill _ = error "Not implemented yet"

isSolved :: Sudoku -> Bool
isSolved (Board board _ _ _ dimension _) =
  (null . filter id . map isEmpty) 
    [board ! (i, j) 
      | i <- [0..dimension], j <- [0..dimension]]

isValid :: Sudoku -> Bool
isValid (Board board lins cols quads dimension shortDimension) =
  null [
    (isValue (board ! (i, j))) 
      || (not . null . filter id) 
           (zipWith3 (and') [lins  ! (i, k) | k <- [0..dimension]] 
                            [cols  ! (j, k) | k <- [0..dimension]] 
                            [quads ! (whichQuadrant (i, j) shortDimension, k) | k <- [0..dimension]])
    | i <- [0..dimension], j <- [0..dimension]]
  where
    and' x y z = x && y && z

derivate :: Sudoku -> (Position, [Value])
derivate _ = error "Not implemented yet"

place :: Sudoku -> Position -> Value -> Sudoku
place _ _ _ = error "Not implemented yet"
