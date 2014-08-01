module Strain (
  keep
, discard
) where

keep :: (a -> Bool) -> [a] -> [a]
keep f = foldr keep' []
  where keep' x xs = if f x
                     then x : xs
                     else xs

discard :: (a -> Bool) -> [a] -> [a]
discard f = keep (not . f)
