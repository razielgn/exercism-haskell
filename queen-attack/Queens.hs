module Queens (
  boardString
, canAttack
) where

import Data.List (intersperse)
import Data.List.Split (chunksOf)

type Position = (Int, Int)

boardString :: Maybe Position -> Maybe Position -> String
boardString w b = present $ map fill grid
  where fill p
          | Just p == w = 'W'
          | Just p == b = 'B'
          | otherwise   = 'O'
        grid = [(x, y) | x <- [0..7], y <- [0..7]]
        present = unlines . map (intersperse ' ') . chunksOf 8

canAttack :: Position -> Position -> Bool
canAttack (x, y) (x', y')
  | x == x'   = True
  | y == y'   = True
  | m == n    = True
  | otherwise = False
  where m = abs $ x - x'
        n = abs $ y - y'
