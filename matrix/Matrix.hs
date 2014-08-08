module Matrix (
  Matrix
, row
, column
, rows
, cols
, shape
, transpose
, reshape
, flatten
, fromString
, fromList
) where

import Control.Arrow ((&&&))
import Data.Char (isSpace)
import Data.List.Split (split, startsWith)
import Data.Vector (Vector)
import qualified Data.Vector as V

data Matrix a = Matrix Int Int (Vector a)
                deriving (Eq, Show)

row :: Int -> Matrix a -> Vector a
row i (Matrix _ w v) = V.slice (w * i) w v

column :: Int -> Matrix a -> Vector a
column i (Matrix h w v) = V.backpermute v range
  where range = V.enumFromStepN i w h

rows :: Matrix a -> Int
rows (Matrix h _ _) = h

cols :: Matrix a -> Int
cols (Matrix _ w _) = w

shape :: Matrix a -> (Int, Int)
shape = rows &&& cols

transpose :: Matrix a -> Matrix a
transpose m@(Matrix h w _) = Matrix w h transposed
  where transposed = V.concatMap (`column` m) range
        range      = V.enumFromN 0 w

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (h, w) (Matrix _ _ v) = Matrix h w v

flatten :: Matrix a -> Vector a
flatten (Matrix _ _ v) = v

fromString :: Read a => String -> Matrix a
fromString = fromList . map (map read . words') . lines
  where splitQuoted  = map rstrip . split (startsWith " \"")
        rstrip       = dropWhile isSpace
        words' xs
          | '"' `elem` xs = splitQuoted xs
          | otherwise     = words xs

fromList :: [[a]] -> Matrix a
fromList l = Matrix height' width' vector'
  where height' = length l
        width'  = if null l then 0 else length $ head l
        vector' = V.fromList $ concat l
