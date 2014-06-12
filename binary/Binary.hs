module Binary (toDecimal) where

import Data.Char (digitToInt)
import qualified Data.Sequence as Seq
import Data.Sequence (foldrWithIndex)

toDecimal :: String -> Int
toDecimal n
  | not valid = 0
  | otherwise = toDecimal' n
  where valid = all (`elem` "01") n
        toDecimal' = foldrWithIndex folder 0 . Seq.fromList . reverse
        folder i d = (+) (2 ^ i * digitToInt d)
