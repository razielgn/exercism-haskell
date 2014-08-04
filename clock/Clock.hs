module Clock (
  fromHourMin
, toString
) where

import Text.Printf (printf)

data Clock = Clock Integer Integer
             deriving (Eq, Show)

instance Num Clock where
  (+) (Clock h1 m1) (Clock h2 m2) = fromHourMin (h1 + h2) (m1 + m2)
  negate (Clock h m) = Clock (pred hours - h) (minutes - m)
  fromInteger = fromHourMin 0
  abs = undefined
  signum = undefined
  (*) = undefined

fromHourMin :: Integer -> Integer -> Clock
fromHourMin h m = Clock h' m'
  where m' = m `mod` minutes
        h' = (h + m `div` minutes) `mod` hours

toString :: Clock -> String
toString (Clock h m) = printf "%02d:%02d" h m

hours :: Integer
hours = 24

minutes :: Integer
minutes = 60
