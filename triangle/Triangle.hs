module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illogical
                  deriving (Eq, Show)

triangleType :: Int -> Int -> Int -> TriangleType
triangleType a b c
  | not possible = Illogical
  | different    = Scalene
  | equal        = Equilateral
  | otherwise    = Isosceles
  where possible  = a + b > c && a + c > b && b + c > a
        equal     = a == b && b == c
        different = a /= b && b /= c && a /= c
