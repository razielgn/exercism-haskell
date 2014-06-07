module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor word = filter (isAnagramOf . lowercase)
  where w = lowercase word
        sw = sort w
        isAnagramOf w' = w /= w' && sw == sort w'
        lowercase = map toLower
