module Garden (
  Plant(..)
, garden
, defaultGarden
, lookupPlants
) where

import Data.Char (ord)
import Data.List (sort, transpose)
import Data.List.Split (chunksOf)
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Show, Eq)

data Garden = GenericGarden [[Plant]]
            | NamedGarden (HashMap Name [Plant])

type Name = String

garden :: [Name] -> String -> Garden
garden names = NamedGarden . Map.fromList . zip (sort names) . chop

defaultGarden :: String -> Garden
defaultGarden = GenericGarden . chop

lookupPlants :: Name -> Garden -> [Plant]
lookupPlants n (GenericGarden p) = p !! index
  where index = ord (head n) - ord 'A'
lookupPlants n (NamedGarden p) = Map.lookupDefault [] n p

chop :: String -> [[Plant]]
chop = toPlants . reorganize . lines
  where reorganize = map concat . transpose . map (chunksOf 2)
        toPlants   = map (map plant)

plant :: Char -> Plant
plant 'C' = Clover
plant 'G' = Grass
plant 'R' = Radishes
plant 'V' = Violets
plant _   = undefined
