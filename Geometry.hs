module Geometry (Geometry, intersectionWithGeometry, triangles) where
import Data.List
import Data.Maybe
import Linear
import Material
import Ray
import Transform
import Vector

-- A geometry
data Geometry = Triangles [Triangle]

-- Intersects with a geometry
intersectionWithGeometry :: Ray -> Transform -> Geometry -> Maybe Vector

intersectionWithGeometry ray tr (Triangles t) = case find isJust (map (intersectionWithTriangle ray tr) t) of
  Nothing -> Nothing
  Just result -> result
  

-- A triangle as three vectors and a normal
type Triangle = (Vector, Vector, Vector, Vector)

-- Creates a triangle geometry
triangles :: [Vector] -> Geometry

triangles vectors = Triangles (triangleList vectors)

-- Creates a triangle list
triangleList :: [Vector] -> [Triangle]

triangleLis [] = Triangles []

triangleList [_] = error("invalid number of triangles")

triangleList [_, _] = error("invalid number of triangles")

triangleList (v1:(v2:(v3:xs))) = (triangle v1 v2 v3):(triangleList xs)

-- Creates a triangle
triangle :: Vector -> Vector -> Vector -> Triangle

triangle v1 v2 v3
  | colinear v1 v2 v3 = error("points are colinear")
  | otherwise = (v1, v2, v3, (cross v1 v2))

-- intersects with  triangle
intersectionWithTriangle :: Ray -> Transform -> Triangle -> Maybe Vector

intersectionWithTriangle ray tr (w1, w2, w3, _) = case solve(system) of
  Just [k, l, m] -> if k >= 0.0 && (withinTriangle v1 v2 v3 result) then Just result else Nothing
    where result = addVectors v1 (combineVectors v2 l v3 m)
  Nothing -> Nothing
  where
    v1 = transformVector tr w1
    v2 = transformVector tr w2
    v3 = transformVector tr w3
    [xq, yq, zq] = v1
    [x1, y1, z1] = subtractVectors v2 v1
    [x2, y2, z2] = subtractVectors v3 v1
    [xp, yp, zp] = getPoint ray
    [xd, yd, zd] = getDirection ray
    system = [[- xd, x1, x2, xp - xq],[- yd, y1, y2, yp - yq],[- zd, z1, z2, zp - zq]]

-- Is within triangle
withinTriangle :: Vector -> Vector -> Vector -> Vector -> Bool

withinTriangle v1 v2 v3 v = l1 > 0.0 && l2 > 0.0 && l3 > 0.0
  where (l1, l2, l3) = barycentric v1 v2 v3 v

-- Calculates the barycentric coordinates
barycentric :: Vector -> Vector -> Vector -> Vector -> (Float, Float, Float)

barycentric [x1, y1, z1] [x2, y2, z2] [x3, y3, z3] [x, y, z] = (l1, l2, l3)
  where det = (y2 - y3) * (x1 - x3) + (x3 - x2) * (y1 - y3)
        l1 = ((y2 - y3) * (x - x3) + (x3 - x2) * (y - y3)) / det
        l2 = ((y3 - y1) * (x - z3) + (x1 - x3) * (y - y3)) / det
        l3 = 1 - l1 - l2
