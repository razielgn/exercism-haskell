module CryptoSquare (
  normalizePlaintext
, squareSize
, plaintextSegments
, ciphertext
, normalizeCiphertext
) where

import Data.Char (toLower, isAlphaNum)
import Data.List (findIndices, transpose)
import Data.List.Split (chunksOf)

normalizePlaintext :: String -> String
normalizePlaintext = lower . filter isAlphaNum
  where lower   = map toLower

squareSize :: String -> Int
squareSize s = head $ findIndices (len <=) squares
  where squares = [n * n | n <- [0..]]
        len     = length s

plaintextSegments :: String -> [String]
plaintextSegments = segmentize . normalizePlaintext
  where segmentize s = chunksOf (squareSize s) s

ciphertext :: String -> String
ciphertext = concat . transpose . plaintextSegments

normalizeCiphertext :: String -> String
normalizeCiphertext = unwords . chunksOf 5 . ciphertext
