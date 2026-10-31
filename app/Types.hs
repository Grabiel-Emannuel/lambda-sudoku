module Types where

import Data.Array.ST (STUArray)

import Data.Int (Int8)

type Index = Int
type Position = (Index, Index)

type Dimension = Int
type Value = Int8

data Sudoku = Board (STUArray Position Value)  -- Board 
                    (STUArray Position Bool)   -- Lines
                    (STUArray Position Bool)   -- Columns
                    (STUArray Position Bool)   -- Quadrants
		    Dimension Dimension
