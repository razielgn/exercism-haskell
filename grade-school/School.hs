module School (School, add, empty, sorted, grade) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (sort)

type Name = String
type Grade = Int
type School = Map Grade [Name]

add :: Grade -> Name -> School -> School
add g name = Map.insertWith (++) g [name]

empty :: School
empty = Map.empty

sorted :: School -> [(Grade, [Name])]
sorted = Map.toAscList . fmap sort

grade :: Grade -> School -> [Name]
grade = Map.findWithDefault []
