module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear y
  | isDivBy 400 = True
  | isDivBy 100 = False
  | isDivBy 4   = True
  | otherwise   = False
  where isDivBy a = mod y a == 0
