module Luhn (
  checkDigit
, digits
, addends
, checksum
, isValid
, create
) where

checkDigit :: Integer -> Integer
checkDigit = (`mod` 10)

digits :: Integer -> [Integer]
digits 0 = []
digits n = let (div_, mod_) = divMod n 10
           in digits div_ ++ [mod_]

addends :: Integer -> [Integer]
addends = double . digits
  where double xs = zipWith fn (range xs) xs
        range xs  = enumFrom $ if even $ length xs then 0 else 1 :: Integer
        fn i n
          | even i    = let n' = n * 2
                        in if n' > 9
                           then n' - 9
                           else n'
          | otherwise = n

checksum :: Integer -> Integer
checksum = checkDigit . sum . addends

isValid :: Integer -> Bool
isValid = (== 0) . checksum

create :: Integer -> Integer
create n = let padded = n * 10
               sum'   = checksum padded
               digit  = if sum' == 0
                        then sum'
                        else 10 - sum'
           in padded + digit
