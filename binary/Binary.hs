module Binary (toDecimal) where

import Data.Char (digitToInt)
import Data.List (foldl')

toDecimal :: String -> Int
toDecimal n
  | not valid = 0
  | otherwise = toDecimal' n
  where valid = all (`elem` "01") n
        toDecimal' = foldl' mapper 0 . zip indexes . reverse
        indexes = [0..] :: [Int]
        mapper acc (i, d) = acc + 2 ^ i * digitToInt d
