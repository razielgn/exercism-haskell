module PrimeFactors (primeFactors) where

import Data.List (sort)

primeFactors :: Integer -> [Integer]
primeFactors x = f' [] x 2
  where f' fs 1 _ = sort fs
        f' fs n d = if n `mod` d == 0
                    then f' (d:fs) (n `div` d) 2
                    else f' fs n (succ d)
