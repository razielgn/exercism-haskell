module PrimeFactors (primeFactors) where

import Data.List (unfoldr)

primeFactors :: Integer -> [Integer]
primeFactors x = unfoldr unfolder (x, 2)
  where unfolder (1, _) = Nothing
        unfolder (n, d)
          | d * d > n      = Just (n, (1, d))
          | n `mod` d == 0 = Just (d, (n `div` d, d))
          | otherwise      = unfolder (n, succ d)
