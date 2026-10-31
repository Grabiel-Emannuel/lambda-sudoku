module Helper where

import Types (Value, Dimension, Index)

isEmpty :: Value -> Bool
isEmpty v = v == 0

isValue :: Value -> Bool
isValue = not . isEmpty

whichQuadrant :: (Index, Index) -> Dimension -> Index
whichQuadrant (line, col) shortDimension =
	(line `div` shortDimension) * shortDimension + (col `mod` shortDimension)	
