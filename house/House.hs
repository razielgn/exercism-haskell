module House (
  rhyme
) where

import Data.List (intercalate)

rhyme :: String
rhyme = unlines strophes
  where strophes = map strophe [0..11]

strophe :: Int -> String
strophe n = intro ++ subject n ++ rest ++ ".\n"
  where intro  = "This is the "
        verses = intercalate "\n" $ map verse [n - 1, n - 2 .. 0]
        rest
          | n == 0    = ""
          | otherwise = "\n" ++ verses

verse :: Int -> String
verse n = "that " ++ verb n ++ " the " ++ subject n

verb :: Int -> String
verb 0  = "lay in"
verb 1  = "ate"
verb 2  = "killed"
verb 3  = "worried"
verb 4  = "tossed"
verb 5  = "milked"
verb 6  = "kissed"
verb 7  = "married"
verb 8  = "woke"
verb 9  = "kept"
verb 10 = "belonged to"
verb _  = undefined

subject :: Int -> String
subject 0  = "house that Jack built"
subject 1  = "malt"
subject 2  = "rat"
subject 3  = "cat"
subject 4  = "dog"
subject 5  = "cow with the crumpled horn"
subject 6  = "maiden all forlorn"
subject 7  = "man all tattered and torn"
subject 8  = "priest all shaven and shorn"
subject 9  = "rooster that crowed in the morn"
subject 10 = "farmer sowing his corn"
subject 11 = "horse and the hound and the horn"
subject _  = undefined
