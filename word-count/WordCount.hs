module WordCount (wordCount) where

import Data.Map.Strict (Map, fromListWith)
import Data.Char (isAlphaNum, toLower)

wordCount :: String -> Map String Int
wordCount = fromListWith (+) . toList  . words . lowercase . transmute
  where toList = map (\x -> (x, 1))
        lowercase = map toLower
        transmute = map (\x -> if isAlphaNum x then x else ' ')
