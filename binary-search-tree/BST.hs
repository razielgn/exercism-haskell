module BST (
  bstLeft
, bstRight
, bstValue
, singleton
, insert
, fromList
, toList
) where

import Data.List (foldl')

data BST a = Empty
           | Node !a !(BST a) !(BST a)

singleton :: Ord a => a -> BST a
singleton x = Node x Empty Empty

bstValue :: Ord a => BST a -> a
bstValue (Node x _ _) = x
bstValue Empty        = undefined

insert :: Ord a => a -> BST a -> BST a
insert x Empty = singleton x
insert x (Node v l r)
  | x > v     = Node v l (insert x r)
  | otherwise = Node v (insert x l) r

bstLeft :: Ord a => BST a -> Maybe (BST a)
bstLeft Empty            = Nothing
bstLeft (Node _ Empty _) = Nothing
bstLeft (Node _ l _)     = Just l

bstRight :: Ord a => BST a -> Maybe (BST a)
bstRight Empty            = Nothing
bstRight (Node _ _ Empty) = Nothing
bstRight (Node _ _ r)     = Just r

fromList :: Ord a => [a] -> BST a
fromList = foldl' (flip insert) Empty

toList :: Ord a => BST a -> [a]
toList Empty        = []
toList (Node x l r) = toList l ++ [x] ++ toList r
