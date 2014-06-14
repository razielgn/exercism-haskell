module Allergies (Allergen(..), isAllergicTo, allergies) where

import Data.Bits (bit, (.&.))

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Enum, Eq, Show)

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo a = (/= 0) . (.&.) bit'
  where bit' = bit $ fromEnum a

allergies :: Int -> [Allergen]
allergies n = filter (`isAllergicTo` n) allAllergies
  where allAllergies = enumFrom Eggs
