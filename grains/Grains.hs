module Grains (square, total) where

square :: Integer -> Integer
square = (`div` 2) . (2 ^)

total :: Integer
total = sum $ map square [1..64]
