module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, isLeapYear)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Calendar.MonthDay (monthLength)

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving (Enum, Eq)

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

type Month = Int
type Year = Integer

meetupDay :: Schedule -> Weekday -> Year -> Month -> Day
meetupDay schedule weekday y m =
  case schedule of
    Teenth -> last teenthDays
    First  -> head days
    Second -> days !! 1
    Third  -> days !! 2
    Fourth -> days !! 3
    Last   -> last days
  where teenthDays        = filterDays $ daysRange 13 19
        days              = filterDays $ daysRange 1 daysInMonth
        daysRange s f     = map (fromGregorian y m) [s..f]
        filterDays        = filter (matchingDay . lastT . toWeekDate)
        matchingDay n     = weekday == toEnum (n - 1)
        daysInMonth       = monthLength (isLeapYear y) m
        lastT (_, _, x)   = x
