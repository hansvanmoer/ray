module Vector (Vector, getX, getY, getZ) where

-- A 3D vector as a (x, y, z) coordinate tuple
type Vector = [Float]

-- Returns the x coordinate
getX :: Vector -> Float

getX vector = vector !! 0

-- Returns the y coordinate
getY :: Vector -> Float

getY vector = vector !! 1

-- Returns the z coordinate
getZ :: Vector -> Float

getZ vector = vector !! 2
