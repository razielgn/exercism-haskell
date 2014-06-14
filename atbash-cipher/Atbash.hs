module Atbash (encode) where

import Data.Char (chr, isAlphaNum, isDigit, ord, toLower)
import Data.List.Split (chunksOf)

encode :: String -> String
encode = partition . transpose . lowercase . filter'
  where transpose = map invert
        filter'   = filter isAlphaNum
        lowercase = map toLower
        partition = unwords . chunksOf 5

invert :: Char -> Char
invert c | isDigit c = c
invert c = chr $ ord 'a' + ord 'z' - ord c
