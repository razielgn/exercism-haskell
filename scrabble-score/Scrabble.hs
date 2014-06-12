module Scrabble (scoreLetter, scoreWord) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Char (toUpper)

scoreLetter :: Char -> Int
scoreLetter l = Map.findWithDefault 0 l' scores
  where l' = toUpper l

scoreWord :: String -> Int
scoreWord = sum . map scoreLetter

scores :: Map Char Int
scores = Map.fromList [('A', 1)
                      ,('B', 3)
                      ,('C', 3)
                      ,('D', 2)
                      ,('E', 1)
                      ,('F', 4)
                      ,('G', 2)
                      ,('H', 4)
                      ,('I', 1)
                      ,('J', 8)
                      ,('K', 5)
                      ,('L', 1)
                      ,('M', 3)
                      ,('N', 1)
                      ,('O', 1)
                      ,('P', 3)
                      ,('Q', 10)
                      ,('R', 1)
                      ,('S', 1)
                      ,('T', 1)
                      ,('U', 1)
                      ,('V', 4)
                      ,('W', 4)
                      ,('X', 8)
                      ,('Y', 4)
                      ,('Z', 10)
                      ]
