module Sublist (
  Sublist(..)
, sublist
) where

data Sublist = Equal
             | Sublist
             | Superlist
             | Unequal
             deriving (Show, Eq)

sublist :: Eq a => [a] -> [a] -> Sublist
sublist xs ys
  | xs == ys       = Equal
  | contains xs ys = Sublist
  | contains ys xs = Superlist
  | otherwise      = Unequal

contains :: Eq a => [a] -> [a] -> Bool
contains _ []          = False
contains patt l@(_:xs) = patt `isPrefixOf` l || contains patt xs
  where isPrefixOf [] _          = True
        isPrefixOf _ []          = False
        isPrefixOf (m:ms) (n:ns) = (m == n) && ms `isPrefixOf` ns
