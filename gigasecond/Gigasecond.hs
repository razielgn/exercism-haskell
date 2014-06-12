module Gigasecond (fromDay) where

import Data.Time.Calendar (Day(..), addDays)

fromDay :: Day -> Day
fromDay = addDays days
  where days       = gigaSecs `div` secsPerDay
        gigaSecs   = 10 ^ (9 :: Integer)
        secsPerDay = 86400
