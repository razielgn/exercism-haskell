module Luhn (
  checkDigit
, digits
, addends
, checksum
, isValid
, create
) where

import Data.Char (digitToInt)

checkDigit :: Integer -> Integer
checkDigit = (`mod` 10)

digits :: Integer -> [Integer]
digits = map (toInteger . digitToInt) . show

addends :: Integer -> [Integer]
addends = double . digits
  where double xs = zipWith fn (range xs) xs
        range xs  = enumFrom $ if even $ length xs then 0 else 1 :: Integer
        fn i n
          | even i    = if n' > 9 then n' - 9 else n'
          | otherwise = n
          where n' = n * 2

checksum :: Integer -> Integer
checksum = checkDigit . sum . addends

isValid :: Integer -> Bool
isValid = (== 0) . checksum

create :: Integer -> Integer
create n = padded + digit
  where padded = n * 10
        sum'   = checksum padded
        digit
          | sum' == 0 = sum'
          | otherwise = 10 - sum'
