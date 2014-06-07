module DNA (count, nucleotideCounts) where

import Data.Map.Strict (Map, fromListWith)

count :: Char -> String -> Int
count c
  | c `elem` "ACTU" = length . filter (== c)
  | otherwise       = error $ "invalid nucleotide " ++ show c

nucleotideCounts :: String -> Map Char Int
nucleotideCounts s = fromListWith (+) $ empty ++ list s
  where list = map (\c -> (c, 1))
        empty = [('A', 0), ('C', 0), ('G', 0), ('T', 0)]
