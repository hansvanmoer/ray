module Matrix (Matrix, identity, diagonal, multiply) where

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

is_diagonal (r, c) = if r == c then True else False

-- A predicate whether a position is in a row
is_in_row :: Int -> (Int, Int) -> Bool

is_in_row row (r, c)
  | row == r = True
  | otherwise = False

-- A predicate whether a position is in a column
is_in_col :: Int -> (Int, Int) -> Bool

is_in_col col (r, c)
  | col == c = True
  | otherwise = False

-- A scalar generator
scalar :: Float -> Bool -> Float

scalar v True = v

scalar v False = 0.0

-- Maps a position onto a value
at_position :: [Float] -> Int -> (Int, Int) -> Float

at_position values cols (r, c) = values !! (r * cols + c)

-- Gets a scalar from a matrix
getElement :: Matrix -> (Int, Int) -> Float

getElement (M _ cols values) (r, c) = values !! ((r + cols)* c)

-- Multiplies a matrix (we do not use Num because * is not commutative)
multiply :: Matrix -> Matrix -> Matrix

multiply (M rows 1 values1) (M 1 cols values2)
  | rows /= cols = error("invalid sizes for matrix multiplication")
  | otherwise = M 1 1 [foldr (+) 0.0 [x * y | x <- values1, y <- values2]]

multiply (M rows1 cols1 values1) (M rows2 cols2 values2)
  | cols1 /= rows2 = error("invalid sizes for matrix multiplication")
  | otherwise = M rows1 cols2 (map (\(r, c) -> (getElement (multiply (rowMatrix (M rows1 cols1 values1) r)  (columnMatrix (M rows2 cols2 values2) c)) (0,0))) (positions rows1 cols2))


-- A row of a matrix
rowMatrix :: Matrix -> Int -> Matrix

rowMatrix (M rows cols values) row = M 1 cols (map (at_position values cols) (filter (is_in_row row) (positions rows cols)))

-- A column of a matrix
columnMatrix :: Matrix -> Int -> Matrix

columnMatrix (M rows cols values) col = M rows 1 (map (at_position values cols) (filter (is_in_col col) (positions rows cols)))
