module Raindrops (convert) where

import PrimeFactors (primeFactors)
import Data.List (nub)

convert :: Integer -> String
convert x = convert' $ translator $ nub $ primeFactors x
  where convert' "" = show x
        convert' s  = s
        translator  = concatMap translate
        translate 3 = "Pling"
        translate 5 = "Plang"
        translate 7 = "Plong"
        translate _ = ""
