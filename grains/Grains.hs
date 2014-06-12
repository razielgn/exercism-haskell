module Grains (square, total) where

square :: Integer -> Integer
square = (2 ^) . pred

total :: Integer
total = 2 ^ 64 - 1
