module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, gregorianMonthLength)
import Data.Time.Calendar.OrdinalDate (mondayStartWeek, fromMondayStartWeek)

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving (Enum, Show)

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth
              deriving (Enum, Show)

type Month = Int
type Year = Integer

meetupDay :: Schedule -> Weekday -> Year -> Month -> Day
meetupDay s w y m = fromMondayStartWeek y w' weekday
  where w' = case s of
               Teenth -> let (weekN, dayN) = firstTDay
                         in if weekday >= dayN then weekN else weekN + 1
               Last   -> let (weekN, dayN) = lastDay
                         in if weekday <= dayN then weekN else weekN - 1
               _      -> let (weekN, dayN) = firstDay
                         in schedule + if weekday >= dayN then weekN else weekN + 1
        weekday   = fromEnum w + 1
        schedule  = fromEnum s
        firstTDay = nthDay 13
        firstDay  = nthDay 1
        lastDay   = nthDay monthLen
        nthDay    = mondayStartWeek . fromGregorian y m
        monthLen  = gregorianMonthLength y m
