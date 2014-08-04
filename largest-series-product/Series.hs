module Series (
  digits
, slices
, largestProduct
) where

import Data.Char (digitToInt)
import Data.List (tails)

digits :: String -> [Int]
digits = map digitToInt

slices :: Int -> String -> [[Int]]
slices n = slice . digits
  where slice xs = map (take n) $ take (length xs - n + 1) (tails xs)

largestProduct :: Int -> String -> Int
largestProduct n s =
  case map product $ slices n s of
    []       -> 1
    products -> maximum products
