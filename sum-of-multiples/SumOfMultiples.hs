module SumOfMultiples (
  sumOfMultiples
, sumOfMultiplesDefault
) where

sumOfMultiplesDefault :: Integer -> Integer
sumOfMultiplesDefault = sumOfMultiples [3, 5]

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples numbers n = sum multiples
  where range     = [1.. n - 1]
        multiples = [x | x <- range, any (x `mulOf`) numbers]
        mulOf x y = x `mod` y == 0
