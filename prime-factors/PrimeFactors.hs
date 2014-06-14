module PrimeFactors (primeFactors) where

import Data.List (unfoldr)

primeFactors :: Integer -> [Integer]
primeFactors x = unfoldr unfolder (x, 2)
  where unfolder (1, _) = Nothing
        unfolder (n, d) = if n `mod` d == 0
                          then Just (d, (n `div` d, 2))
                          else unfolder (n, succ d)
