module Ray (getDirection, getPoint, Ray, ray) where
import Vector

data Ray = R Vector Vector

-- Creates a new ray
ray :: Vector -> Vector -> Ray

ray point direction
  | not (isNullVector direction) = (R point direction)
  | otherwise = error("ray direction can not be the null vector")

getPoint :: Ray -> Vector

getPoint (R point _) = point

getDirection :: Ray -> Vector

getDirection (R _ dir) = dir
