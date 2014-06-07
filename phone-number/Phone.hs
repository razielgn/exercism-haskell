module Phone (areaCode, number, prettyPrint) where

import Data.Char (isNumber)
import Data.List.Split (splitPlaces)

number :: String -> String
number n
  | isCorrect clean  = clean
  | isAmerican clean = tail clean
  | otherwise        = invalid
  where isCorrect n'  = length n' == 10
        isAmerican n' = length n' == 11 && head clean == '1'
        clean = filter isNumber n
        invalid = replicate 10 '0'

areaCode :: String -> String
areaCode = take 3

prettyPrint :: String -> String
prettyPrint n = let [area, prefix, poss] = splitPlaces places clean
                in "(" ++ area ++ ") " ++ prefix ++ "-" ++ poss
  where clean = number n
        places = [3, 3, 4] :: [Int]
