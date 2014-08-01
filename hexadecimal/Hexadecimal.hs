module Hexadecimal (
  hexToInt
) where

import Control.Monad (liftM2)
import Data.List (foldl')
import Data.Maybe (fromMaybe)

hexToInt :: String -> Integer
hexToInt = unbox . combine . zip indexes . digits . reverse
  where digits  = map hex
        indexes = [16 ^ i | i <- [0..] :: [Integer]]
        combine = foldl' multiply (Just 0)
        multiply int (i, digit) = int .+. (digit .*. Just i)
        (.+.) = liftM2 (+)
        (.*.) = liftM2 (*)
        unbox = fromMaybe 0

hex :: Char -> Maybe Integer
hex '0' = Just 0
hex '1' = Just 1
hex '2' = Just 2
hex '3' = Just 3
hex '4' = Just 4
hex '5' = Just 5
hex '6' = Just 6
hex '7' = Just 7
hex '8' = Just 8
hex '9' = Just 9
hex 'a' = Just 10
hex 'b' = Just 11
hex 'c' = Just 12
hex 'd' = Just 13
hex 'e' = Just 14
hex 'f' = Just 15
hex _   = Nothing
