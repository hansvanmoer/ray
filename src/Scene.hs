module Scene where

import Json
import Vector

data Triangle = T Vector3 Vector3 Vector3

triangle :: Vector3 -> Vector3 -> Vector3 -> Triangle
triangle = T

data Geometry = Triangles [Triangle]

triangles :: [Triangle] -> Geometry
triangles = Triangles

newtype Scene = S [Geometry]

scene :: [Geometry] -> Scene
scene = S

parseScene :: String -> Either JsonError Scene
parseScene input = parseJson input >> return (S [])
      
