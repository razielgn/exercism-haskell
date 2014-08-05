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

import Data.Char (isSpace)
import Data.List.Split (split, startsWith)
import Data.Maybe (fromMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as V

newtype Matrix a = Matrix (Vector (Vector a))
                   deriving (Eq, Show)

row :: Int -> Matrix a -> Vector a
row i (Matrix m) = m V.! i

column :: Int -> Matrix a -> Vector a
column i (Matrix m) = V.map (V.! i) m

rows :: Matrix a -> Int
rows (Matrix m) = V.length m

cols :: Matrix a -> Int
cols (Matrix m) = V.length firstRow
  where firstRow = fromMaybe V.empty $ m V.!? 0

shape :: Matrix a -> (Int, Int)
shape m = (rows m, cols m)

transpose :: Matrix a -> Matrix a
transpose (Matrix m) = Matrix $ go m
  where go vs
          | V.null vs       = V.empty
          | V.null firstRow = go vss
          | otherwise       = x `V.cons` V.map V.head vss `V.cons` go (xs `V.cons` V.map V.tail vss)
          where vss      = V.tail vs
                firstRow = V.head vs
                x        = V.head firstRow
                xs       = V.tail firstRow

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (_, c) (Matrix m) = Matrix $ V.concatMap splitter m
  where splitter v = V.fromList [v1, v2]
          where (v1, v2) = V.splitAt c v

flatten :: Matrix a -> Vector a
flatten (Matrix m) = V.foldl' (V.++) V.empty m

fromString :: Read a => String -> Matrix a
fromString = Matrix . createMatrix
  where createMatrix = V.fromList . map createRow . lines
        createRow    = V.fromList . map read . words'
        splitQuoted  = map rstrip . split (startsWith " \"")
        rstrip       = dropWhile isSpace
        words' xs
          | '"' `elem` xs = splitQuoted xs
          | otherwise     = words xs

fromList :: [[a]] -> Matrix a
fromList = Matrix . V.fromList . map V.fromList
