module PrimeFactors (primeFactors) where

import Data.List (unfoldr)

primeFactors :: Integer -> [Integer]
primeFactors x = unfoldr unfolder (x, 2)
  where unfolder (1, _) = Nothing
        unfolder (n, d)
          | stop n d       = Just (n, (1, d))
          | n `mod` d == 0 = Just (d, (n `div` d, 2))
          | otherwise      = unfolder (n, succ d)
        stop n d = fromIntegral d > sqrt (fromIntegral n)
