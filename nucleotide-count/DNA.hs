module DNA (count, nucleotideCounts) where

import qualified Data.Map.Strict as Map
import Data.List (foldl')

count :: Char -> String -> Int
count c
  | c `elem` "ACTU" = length . filter (== c)
  | otherwise       = error $ "invalid nucleotide '" ++ c : "'"

nucleotideCounts :: String -> Map.Map Char Int
nucleotideCounts = foldl' (flip insertion) empty
  where insertion b = Map.insertWith (+) b 1
        empty       = Map.fromList [('A', 0), ('C', 0), ('G', 0), ('T', 0)]
