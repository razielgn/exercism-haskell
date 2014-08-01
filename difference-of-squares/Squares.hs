module Squares (
  sumOfSquares
, squareOfSums
, difference
) where

sumOfSquares :: (Num a, Enum a) => a -> a
sumOfSquares n = sum $ map square [1..n]

squareOfSums :: (Num a, Enum a) => a -> a
squareOfSums n = square $ sum [1..n]

difference :: (Num a, Enum a) => a -> a
difference n = squareOfSums n - sumOfSquares n

square :: Num a => a -> a
square = (^ two)
  where two = 2 :: Integer
