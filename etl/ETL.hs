module ETL (transform) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Char (toLower)

transform :: Map Int [String] -> Map String Int
transform = Map.fromList . Map.foldrWithKey f []
  where f score values ary = foldr (f' score) ary values
        f' score val = (:) (lowercase val, score)
        lowercase = map toLower
