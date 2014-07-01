module Robot (
  Bearing(..)
, Robot
, mkRobot
, coordinates
, simulate
, bearing
, turnRight
, turnLeft
) where

import Data.List (foldl')

data Bearing = North
             | East
             | South
             | West
             deriving (Show, Eq, Enum)

type Position = (Int, Int)

data Robot = Robot !Bearing !Position
             deriving (Show, Eq)

mkRobot :: Bearing -> Position -> Robot
mkRobot = Robot

coordinates :: Robot -> Position
coordinates (Robot _ p) = p

bearing :: Robot -> Bearing
bearing (Robot b _) = b

simulate :: Robot -> String -> Robot
simulate = foldl' action

action :: Robot -> Char -> Robot
action r cmd =
  case cmd of
    'R' -> mkRobot (turnRight b) c
    'L' -> mkRobot (turnLeft b) c
    'A' -> mkRobot b (move b c)
    _   -> mkRobot b c
  where b = bearing r
        c = coordinates r
        move North (x, y) = (x, y + 1)
        move East  (x, y) = (x + 1, y)
        move South (x, y) = (x, y - 1)
        move West  (x, y) = (x - 1, y)

turnRight :: Bearing -> Bearing
turnRight West = North
turnRight b = succ b

turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft b = pred b
