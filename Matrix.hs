module Matrix (Matrix, identity) where

-- A matrix with a number of rows, columns and an array of data
data Matrix = M Int Int [Float]

-- The square identity matrix
identity :: Int -> Matrix

identity size = diagonal size 1.0

-- A diagonal matrix
diagonal :: Int -> Float -> Matrix

diagonal size value =  M size size (map ((scalar 1.0) . is_diagonal) (positions size size))

-- The position matrix
positions :: Int -> Int -> [(Int, Int)]

positions rows cols = [(r, c) | r <- [0..rows], c <- [0..cols]]

-- A predicate whether a position is on the diagonal
is_diagonal :: (Int, Int) -> Bool

is_diagonal (x, y) = if x == y then True else False

-- A scalar generator
scalar :: Float -> Bool -> Float

scalar v True = v

scalar v False = 0.0

