module Vector (Vector, addVectors, colinear, combineVectors, cross, divideByScalar, getX, getY, getZ, isNullVector, multiplyByScalar, subtractVectors, zero) where

-- A 3D vector as a (x, y, z) coordinate tuple
type Vector = [Float]

-- The zero vector
zero :: Int -> Vector

zero length = replicate length 0.0

-- Whether the vector is the null vector
isNullVector :: Vector -> Bool

isNullVector [] = True

isNullVector [x] = x == 0.0

isNullVector (x:xs) = (x == 0.0) && (isNullVector xs)

-- Adds vectors
addVectors :: Vector -> Vector -> Vector

addVectors left right
  | (length left) == (length right) = zipWith (+) left right
  | otherwise = error("vectors should be of same length")

-- Adds vectors
subtractVectors :: Vector -> Vector -> Vector

subtractVectors left right
  | (length left) == (length right) = zipWith (-) left right
  | otherwise = error("vectors should be of same length")
    
-- Multiply vector with scalar
multiplyByScalar :: Vector -> Float -> Vector

multiplyByScalar left right = map (* right) left

-- A linear combination of vectors
combineVectors :: Vector -> Float -> Vector -> Float -> Vector

combineVectors left leftScalar right rightScalar = addVectors (multiplyByScalar left leftScalar) (multiplyByScalar right rightScalar)

-- Negates a vector
negate :: Vector -> Vector

negate v = multiplyByScalar v (- 1.0)

-- Divide vector by a scalar
divideByScalar :: Vector -> Float -> Vector

divideByScalar left right = multiplyByScalar left (1.0 / right)

-- Returns the x coordinate
getX :: Vector -> Float

getX vector = vector !! 0

-- Returns the y coordinate
getY :: Vector -> Float

getY vector = vector !! 1

-- Returns the z coordinate
getZ :: Vector -> Float

getZ vector = vector !! 2

-- True if three vectors are colinear
colinear :: Vector -> Vector -> Vector -> Bool

colinear [x1, y1, z1] [x2, y2, z2] [x3, y3, z3] = x1 * y2 * z3 +
                                                  y1 * z2 * x1 +
                                                  z1 * x2 * y3 -
                                                  x3 * y2 * z1 -
                                                  x2 * y1 * z3 -
                                                  y3 * z2 * x1 == 0.0
-- Calculates the cross product
cross :: Vector -> Vector -> Vector

cross [x1, y1, z1] [x2, y2, z2] = [(y1 * z2 - y2 * z1), - (x1 * z2 - x2 * z1), (x1 * y2 - x2 * y1)] 
