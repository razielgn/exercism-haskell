module Raindrops (convert) where

convert :: Integer -> String
convert x = convert' translator
  where convert' "" = show x
        convert' s  = s
        translator  = concatMap mapper [3, 5, 7]
        mapper d    = if x `mod` d == 0 then translate d else ""
        translate 3 = "Pling"
        translate 5 = "Plang"
        translate 7 = "Plong"
        translate _ = ""
