module Types where

import Data.Array.Unboxed (UArray)
import Data.Int (Int8)

type Index = Int
type BoardPosition = (Index, Index)
type DerivatePosition = (Index, Index, Index)

type Dimension = Int
type Value = Int8

type Derivate  = UArray DerivatePosition Bool
type DerivateValue = (BoardPosition, [Value])

data Sudoku = Board (UArray BoardPosition Value)  -- Board 
                    (UArray BoardPosition Bool)   -- Lines
                    (UArray BoardPosition Bool)   -- Columns
                    (UArray BoardPosition Bool)   -- Quadrants
                    Dimension Dimension

instance Eq Sudoku where
  (Board board lins cols quads dimension shortDimension) == 
    (Board board' lins' cols' quads' dimension' shortDimension') =
         (board == board') && (lins == lins') && (cols == cols')
      && (quads == quads') && (dimension == dimension') && (shortDimension == shortDimension')
