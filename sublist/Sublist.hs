module Sublist (
  Sublist(..)
, sublist
) where

import Data.Vector (Vector)
import qualified Data.Vector as V

data Sublist = Equal
             | Sublist
             | Superlist
             | Unequal
             deriving (Show, Eq)

sublist :: Eq a => [a] -> [a] -> Sublist
sublist xs ys
  | xs == ys         = Equal
  | contains xs' ys' = Sublist
  | contains ys' xs' = Superlist
  | otherwise        = Unequal
  where xs' = V.fromList xs
        ys' = V.fromList ys

contains :: Eq a => Vector a -> Vector a -> Bool
contains a b
  | V.null a  = True
  | V.null b  = False
  | otherwise = check $ indeces b
  where indeces = V.elemIndices (V.head a)
        check   = V.foldl' (\c i -> c || (i + aLen <= bLen && a == V.slice i aLen b)) False
        aLen    = V.length a
        bLen    = V.length b
