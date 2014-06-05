module Bob (responseFor) where

import Data.Char (isUpper, isAlpha, isSpace)
import Data.List (isSuffixOf)

responseFor :: String -> String
responseFor sentence
  | isNothing  = "Fine. Be that way!"
  | isShouting = "Woah, chill out!"
  | isQuestion = "Sure."
  | otherwise  = "Whatever."
  where areAllUpper   = all isUpper
        isQuestion    = "?" `isSuffixOf` sentence
        isShouting    = saysSomething sentence && areAllUpper (filter isAlpha sentence)
        saysSomething = any isAlpha
        isNothing     = all isSpace sentence
