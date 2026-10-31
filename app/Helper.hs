module Helper where

import Data.Array.Unboxed (array, (!))
import Types (BoardPosition, Derivate, Dimension, Index, Sudoku(Board), Value)

isEmpty :: Value -> Bool
isEmpty v = v == 0

isValue :: Value -> Bool
isValue = not . isEmpty

whichQuadrant :: BoardPosition -> Dimension -> Index
whichQuadrant (line, col) shortDimension =
  ((pred line) `div` shortDimension) * shortDimension + ((pred col) `div` shortDimension) + 1

derivate :: Sudoku -> Derivate
derivate (Board board lins cols quads dimension shortDimension) =
  (array ((1,1,1), (dimension, dimension, dimension)) . concatMap derivate')
    [if   isValue  (board ! (i, j)) 
      then ((i, j), [k == (fromIntegral (board ! (i, j))) | k <- [1..dimension]]) 
      else ((i, j), zipWith3 (and') [lins  ! (i, k) | k <- [1..dimension]] 
                                    [cols  ! (j, k) | k <- [1..dimension]] 
                                    [quads ! (whichQuadrant (i, j) shortDimension, k) 
                                      | k <- [1..dimension]]
        )
      | i <- [1..dimension], j <- [1..dimension]]
  where
    and' x y z = x && y && z
    derivate' ((i,j), values) = [((i,j,k), v) | (k,v) <- (zip [1..] values)] 

hasSame :: Derivate -> Sudoku -> Bool
hasSame derivated sudoku@(Board _ _ _ _ dimension _) = 
  or  [hasSame' derivated sudoku (i,j) 
        | i <- [1..dimension], j <- [1..dimension], 
          (length . filter id) [derivated ! (i, j, k) | k <- [1..dimension]] == 1]

hasSame' :: Derivate -> Sudoku -> BoardPosition -> Bool
hasSame' derivated sudoku (i,j) =
     hasSameLine derivated sudoku (i,j) 
  || hasSameCol  derivated sudoku (i,j)
  || hasSameQuad derivated sudoku (i,j)

hasSameLine :: Derivate -> Sudoku -> BoardPosition -> Bool
hasSameLine derivated (Board _ _ _ _ dimension _) (i,j) =
  (not . null . filter id . map ( == [derivated ! (i,j,k) | k <- [1..dimension]])) 
    [[derivated ! (i,j',k) | k <- [1..dimension]] | j' <- [succ j..dimension]]

hasSameCol :: Derivate -> Sudoku -> BoardPosition -> Bool
hasSameCol derivated (Board _ _ _ _ dimension _) (i,j) =
  (not . null . filter id . map ( == [derivated ! (i,j,k) | k <- [1..dimension]])) 
    [[derivated ! (i',j,k) | k <- [1..dimension]] | i' <- [succ i..dimension]]

hasSameQuad :: Derivate -> Sudoku -> BoardPosition -> Bool
hasSameQuad derivated (Board _ _ _ _ dimension shortDimension) (i,j) =
  (not . null . filter id . map ( == [derivated ! (i,j,k) | k <- [1..dimension]])) 
    [[derivated ! (i',j',k) | k <- [1..dimension]] 
      | i' <- [1..dimension], j' <- [1..dimension], 
        (i', j') > (i,j), whichQuadrant (i',j') shortDimension == quad]
  where
    quad = whichQuadrant (i,j) shortDimension

hasNone :: Derivate -> Sudoku -> Bool
hasNone derivated (Board _ _ _ _ dimension _) = 
  (not . null . filter id) [
    (null . filter id) [derivated ! (i, j, k) | k <- [1..dimension]] 
                         | i <- [1..dimension], j <- [1..dimension]]

isValidDerivate :: Sudoku -> Derivate -> Bool
isValidDerivate sudoku derivated =
     ( not . hasSame derivated ) sudoku
  && ( not . hasNone derivated ) sudoku 
