module Clock (
  fromHourMin
, toString
) where

import Text.Printf (printf)

newtype Clock = Clock Integer
                deriving (Eq, Show)

instance Num Clock where
  (+) (Clock m1) (Clock m2) = fromInteger $ m1 + m2
  negate (Clock m) = Clock $ day - m
  fromInteger = fromHourMin 0
  (*) = undefined
  abs = undefined
  signum = undefined

fromHourMin :: Integer -> Integer -> Clock
fromHourMin h m = Clock m'
  where m' = (h * hour + m) `mod` day

toString :: Clock -> String
toString (Clock m) = printf format h' m'
  where format   = "%02d:%02d"
        (h', m') = m `divMod` hour

hour :: Integer
hour = 60

day :: Integer
day = 1440
