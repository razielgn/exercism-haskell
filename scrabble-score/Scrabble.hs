module Scrabble (scoreLetter, scoreWord) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Char (toUpper)

scoreLetter :: Char -> Integer
scoreLetter l = first $ Map.filter (elem l') scores
  where l' = toUpper l
        first = head . Map.keys

scoreWord :: String -> Integer
scoreWord = sum . map scoreLetter

scores :: Map Integer String
scores = Map.fromList [(1,  "AEIOULNRST")
                      ,(2,  "DG")
                      ,(3,  "BCMP")
                      ,(4,  "FHVWY")
                      ,(5,  "K")
                      ,(8,  "JX")
                      ,(10, "QZ")
                      ]
