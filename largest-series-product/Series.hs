module Series (
  digits
, slices
, largestProduct
) where

import Data.Char (digitToInt)

digits :: String -> [Int]
digits = map digitToInt

slices :: Int -> String -> [[Int]]
slices n s
  | n == 0        = []
  | length s <  n = []
  | otherwise     = digits (take n s) : slices n (tail s)

largestProduct :: Int -> String -> Int
largestProduct n = maximum' 1 . map product . slices n
  where maximum' x [] = x
        maximum' _ xs = maximum xs
