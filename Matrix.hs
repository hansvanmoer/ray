module Matrix (Matrix, addMatrices, column, divideByScalar, fromList, getColumn, getElement, identity, multiplyMatrices, multiplyWithScalar, positions, square, subtractMatrices, zero) where

-- A matrix with a number of rows, columns and an array of data
data Matrix = M Int Int [Float]

-- Creates a checked
fromList :: Int -> Int -> [Float] -> Matrix

fromList rows cols values
  | (length values) == rows * cols = (M rows cols values)
  | otherwise = error("bad value list length")

-- A square matrix
square :: Int -> [Float] -> Matrix

square size values = fromList size size values

-- The identity matrix
identity :: Int -> Matrix

identity size = M size size [if r == c then 1.0 else 0.0 | (r, c) <- (positions size size)]

-- The zero matrix
zero :: Int -> Int -> Matrix

zero rows cols = M rows cols (replicate (rows * cols) 0.0)

-- A column matrix
column :: [Float] -> Matrix

column values = M (length values) 1 values

-- Get an element from the matrix
getElement :: Matrix -> (Int, Int) -> Float

getElement (M rows cols values) (row, col)
  | row < 0 || row >= rows || col < 0 || col >= cols = error("bad index")
  | otherwise = values !! (row * cols + col)


-- Gets a row
getRow :: Matrix -> Int -> [Float]

getRow (M rows cols values) row
  | row >= 0 && row < rows = [values !! i | i <- [start..end]]
  | otherwise = error("bad row index")
  where
    start = row * cols
    end = (row + 1) * cols

-- Gets a column
getColumn :: Matrix -> Int -> [Float]

getColumn (M rows cols values) col
  | col >= 0 && col < cols = [values !! i | i <- [0..len], (i `rem` cols) == col]
  | otherwise = error("bad column index")
  where
    len = rows * cols

-- Adds two matrices
addMatrices :: Matrix -> Matrix -> Matrix

addMatrices left right = byElementOperator (+) left right

-- Subtracts two matrices
subtractMatrices :: Matrix -> Matrix -> Matrix

subtractMatrices left right = byElementOperator (-) left right

-- Multiplies a matrix with a scalar
multiplyWithScalar :: Matrix -> Float -> Matrix

multiplyWithScalar left right = scalarOperator (*) left right

-- Divide a matrix by a scalar
divideByScalar :: Matrix -> Float -> Matrix

divideByScalar left right = scalarOperator (/) left right

-- Matrix multuplication
multiplyMatrices :: Matrix -> Matrix -> Matrix

multiplyMatrices (M lRows lCols lValues) (M rRows rCols rValues)
  | lCols == rRows = matrixOperator multiplicationOperator lRows rCols (M lRows lCols lValues) (M rRows rCols rValues)
  | otherwise = error("bad matrix dimensions for multiplication")
    
-- A simple scalar to square matrix operator
scalarOperator :: (Float -> Float -> Float) -> Matrix -> Float -> Matrix

scalarOperator op (M rows cols values) right
  | rows == cols = (M rows cols (map (* right) values))
  | otherwise = error("matrix should be square")

-- A simple by element operator
byElementOperator :: (Float -> Float -> Float) -> Matrix -> Matrix -> Matrix

byElementOperator scalarOp (M lRows lCols lValues) (M rRows rCols rValues)
  | lRows == rRows && lCols == rCols = (M lRows lCols (zipWith scalarOp lValues rValues))
  | otherwise = error("matrices should have same dimension")

-- An operator that uses the entire matrix
matrixOperator :: (Matrix -> Matrix -> (Int, Int) -> Float) -> Int -> Int -> Matrix -> Matrix -> Matrix

matrixOperator op rows cols left right = M rows cols (map (op left right) (positions rows cols))

-- Generates a list of all positions in the matrix, in row order
positions :: Int -> Int -> [(Int, Int)]

positions rows cols = [(r, c) | r <- [0..rows], c <- [0..cols]]

-- Matrix multiplication operator
multiplicationOperator :: Matrix -> Matrix -> (Int, Int) -> Float

multiplicationOperator left right (r, c) = foldr (+) 0.0 (zipWith (*) (getRow left r) (getColumn right c))
