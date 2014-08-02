module Hexadecimal (
  hexToInt
) where

import Data.Char (isHexDigit, digitToInt)

hexToInt :: String -> Int
hexToInt = combine 0 . map hex
  where combine acc []     = acc
        combine acc (x:xs) = case x of
          Just n  -> (combine $! acc * 16 + n) xs
          Nothing -> 0

hex :: Char -> Maybe Int
hex h
  | isHexDigit h = Just $ digitToInt h
  | otherwise    = Nothing
