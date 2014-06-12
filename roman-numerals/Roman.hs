module Roman (numerals) where

import Data.List (find)

numerals :: Int -> String
numerals n = case tuple of
               Just (q, sym) -> sym ++ numerals (n - q)
               Nothing       -> ""
  where tuple = find (\(q, _) -> n >= q) symbols

symbols :: [(Int, String)]
symbols = [(1000, "M")
          ,(900,  "CM")
          ,(500,  "D")
          ,(400,  "CD")
          ,(100,  "C")
          ,(90,   "XC")
          ,(50,   "L")
          ,(40,   "XL")
          ,(10,   "X")
          ,(9,    "IX")
          ,(5,    "V")
          ,(4,    "IV")
          ,(1,    "I")
          ]
