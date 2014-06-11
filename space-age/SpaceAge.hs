module SpaceAge (Planet(..), ageOn) where

data Planet = Earth
            | Mercury
            | Venus
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

type Seconds = Integer
type SolarYears = Float

ageOn :: Planet -> Seconds -> SolarYears
ageOn p s = fromIntegral s / period p / secondsInADay
  where period Earth   = 365.25
        period Jupiter = earthDays * 11.862615
        period Mars    = earthDays * 1.8808158
        period Mercury = earthDays * 0.2408467
        period Neptune = earthDays * 164.79132
        period Saturn  = earthDays * 29.447498
        period Uranus  = earthDays * 84.016846
        period Venus   = earthDays * 0.61519726
        earthDays      = period Earth
        secondsInADay  = 86400
