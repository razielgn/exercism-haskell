module LinkedList (
  datum
, fromList
, isNil
, new
, next
, nil
, reverseLinkedList
, toList
) where

import Prelude hiding (reverse)
import Data.Foldable (Foldable, foldMap, foldl')
import Data.Monoid (mempty, mappend)

data List a = Nil | Cons a (List a)

instance Foldable List where
  foldMap _ Nil         = mempty
  foldMap f (Cons x xs) = f x `mappend` foldMap f xs

nil :: List a
nil = Nil

new :: a -> List a -> List a
new = Cons

isNil :: List a -> Bool
isNil Nil = True
isNil _   = False

datum :: List a -> a
datum Nil        = error "called datum on empty list"
datum (Cons x _) = x

next :: List a -> List a
next Nil         = error "called next on empty list"
next (Cons _ xs) = xs

toList :: List a -> [a]
toList Nil         = []
toList (Cons x xs) = x : toList xs

fromList :: [a] -> List a
fromList = foldr new nil

reverseLinkedList :: List a -> List a
reverseLinkedList = foldl' (flip new) nil
