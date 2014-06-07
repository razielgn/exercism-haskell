module Beer (sing, verse) where

import Data.Char (toUpper, isSpace)

sing :: Integer -> Integer -> String
sing from to = strip $ unlines verses
  where strip  = dropWhile isSpace
        verses = reverse $ map verse [to..from]

verse :: Integer -> String
verse n = capitalize $ unlines [firstSentence, secondSentence]
  where firstSentence = beersOnTheWall n ++ ", " ++ bottle n ++ "."
        secondSentence
          | n == 0    = "Go to the store and buy some more, " ++ beersOnTheWall 99 ++ "."
          | otherwise = "Take " ++ pronoun ++ " down and pass it around, " ++ beersOnTheWall (n - 1) ++ "."
        pronoun
          | n > 1     = "one"
          | otherwise = "it"
        beersOnTheWall n' = bottle n' ++ " on the wall"
        capitalize "" = ""
        capitalize (x:xs) = toUpper x : xs
        bottle 1  = str 1 ++ " bottle of beer"
        bottle n' = str n' ++ " bottles of beer"
        str :: Integer -> String
        str 0 = "no more"
        str n' = show n'
