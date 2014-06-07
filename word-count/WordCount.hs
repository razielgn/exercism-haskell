module WordCount (wordCount) where

import qualified Data.Map.Strict as Map
import Data.Char (isAlphaNum, toLower)
import Data.List (foldl')

wordCount :: String -> Map.Map String Int
wordCount = count Map.empty . words . lowercase . transmute
  where count = foldl' (flip insertion)
        insertion w = Map.insertWith (+) w 1
        lowercase = map toLower
        transmute = map (\x -> if isAlphaNum x then x else ' ')
