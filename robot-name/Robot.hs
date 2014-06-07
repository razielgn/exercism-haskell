module Robot (robotName, mkRobot, resetName) where

import System.Random (randomRIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Monad (liftM)

type Robot = IORef String

mkRobot :: IO Robot
mkRobot = randomName >>= newIORef

robotName :: Robot -> IO String
robotName = readIORef

resetName :: Robot -> IO ()
resetName r = randomName >>= writeIORef r

randomName :: IO String
randomName = do letters <- liftM (replicate 2) randomLetters
                numbers <- liftM (replicate 3) randomNumbers
                return $ letters ++ numbers
  where randomLetters = randomRIO ('A', 'Z')
        randomNumbers = randomRIO ('0', '9')
