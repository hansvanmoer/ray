module Matrix (determinant, matrix1, matrix2, matrix2x3, matrix3, matrix3x4, matrix4, multiply, rows3, transpose, unit, testMatrix) where

import qualified AdditiveGroup as AG
import qualified Array as A
import qualified Field as F
import qualified Vector as V

import Test

type Matrix1 = A.Array1 (A.Array1 Float)

type Matrix2 = A.Array2 (A.Array2 Float)

type Matrix3 = A.Array3 (A.Array3 Float)

type Matrix4 = A.Array4 (A.Array4 Float)

matrix1 :: (F.Field a) => a -> A.Array1 (A.Array1 a)
matrix1 e00 = A.array1 (A.array1 e00)

matrix2 :: (F.Field a) =>
           a -> a ->
           a -> a -> A.Array2 (A.Array2 a)
matrix2 e00 e01 e10 e11 = A.array2
  (A.array2 e00 e01)
  (A.array2 e10 e11)

matrix2x3 :: (F.Field a) =>
  a -> a -> a ->
  a -> a -> a -> A.Array2 (A.Array3 a)
matrix2x3 e00 e01 e02 e10 e11 e12 = A.array2 (A.array3 e00 e01 e02) (A.array3 e10 e11 e12)

matrix3 :: (F.Field a) =>
           a -> a -> a ->
           a -> a -> a ->
           a -> a -> a -> A.Array3 (A.Array3 a)
matrix3 e00 e01 e02 e10 e11 e12 e20 e21 e22 = A.array3
  (A.array3 e00 e01 e02)
  (A.array3 e10 e11 e12)
  (A.array3 e20 e21 e22)

rows3 :: A.Array3 a -> A.Array3 a -> A.Array3 a -> A.Array3 (A.Array3 a)
rows3 = A.array3
  
matrix3x4 :: (F.Field a) =>
          a -> a -> a -> a ->
          a -> a -> a -> a ->
          a -> a -> a -> a -> A.Array3 (A.Array4 a)
matrix3x4 e00 e01 e02 e03 e10 e11 e12 e13 e20 e21 e22 e23 = A.array3
  (A.array4 e00 e01 e02 e03)
  (A.array4 e10 e11 e12 e13)
  (A.array4 e20 e21 e22 e23)


matrix4 :: (F.Field a) =>
           a -> a -> a -> a ->
           a -> a -> a -> a ->
           a -> a -> a -> a ->
           a -> a -> a -> a -> A.Array4 (A.Array4 a)
matrix4 e00 e01 e02 e03 e10 e11 e12 e13 e20 e21 e22 e23 e30 e31 e32 e33 = A.array4
  (A.array4 e00 e01 e02 e03)
  (A.array4 e10 e11 e12 e13)
  (A.array4 e20 e21 e22 e23)
  (A.array4 e30 e31 e32 e33)

size :: (A.Array a, A.Array b) => a (b c) -> (Int, Int)
size matrix = (A.size matrix, A.size (A.get 0 matrix))

getRow :: (A.Array a, A.Array b) => Int -> a (b c) -> b c
getRow = A.get

get :: (A.Array a, A.Array b) => (Int, Int) -> a (b c) -> c
get (r, c) matrix = A.get c (A.get r matrix)

unit :: (A.Array a, F.Field b) => a (a b)
unit = A.fromFunction (\r -> A.fromFunction (\c -> valueAt r c))
  where
    valueAt r c = if r == c
                  then
                    F.identity
                  else
                    AG.identity

transpose :: (A.Array a, A.Array b) => a (b c) -> b (a c)
transpose m = A.fromFunction (\r -> A.fromFunction (\c -> valueAt r c))
  where
    valueAt r c = A.get r (A.get c m)

multiply :: (A.Array a, F.Field b) =>  a (a b) -> a (a b) -> a (a b)
multiply left right = A.fromFunction (\r -> A.fromFunction (\c -> valueAt r c))
  where
    valueAt r c = V.dot (A.get r left) (A.get c (transpose right))

determinant :: (A.Array a, F.Field b) => a (a b) -> b
determinant m = determinantFrom (A.size m) (\p -> get p m)

determinantFrom :: (F.Field a) => Int -> ((Int, Int) -> a) -> a
determinantFrom 1 f = f (0,0)
determinantFrom s f = determinantFromAt 0 s f
  where
    sign i v = if mod i 2 == 0 then v else AG.invert v
    subMatrix i f = (\(r, c) -> f (r + 1, if c < i then c else c + 1))
    determinantFromAt i s f =
      let
        value = sign i (F.multiply (f (0, i)) (determinantFrom (s - 1) (subMatrix i f)))
      in
        if i == (s - 1)
        then value
        else AG.add value (determinantFromAt (i + 1) s f)

findPivot :: (A.Array a, A.Array b, Ord c, AG.AdditiveGroup c) => Int -> a (b c) -> Maybe Int
findPivot index matrix = A.scanFrom index isPivot (pure AG.identity) matrix
  where
    isPivot prevRow row = lessThanAbs (A.get index prevRow) (A.get index row)

lessThanAbs :: (AG.AdditiveGroup a, Ord a) => a -> a -> Bool
lessThanAbs l r =
  let
    lAbs = AG.absolute l
    rAbs = AG.absolute r
  in
    lAbs < rAbs

reduceRow :: (A.Array a, A.Array b, F.Field c) => Int -> Int -> a (b c) -> b c
reduceRow index pivot matrix =
  let
    pivotRow = A.get pivot matrix
    pivotValue = A.get pivot pivotRow
    indexRow = A.get index matrix
    indexValue = A.get pivot indexRow
  in
    pure AG.subtract <*> fmap (F.multiply pivotValue) indexRow <*> fmap (F.multiply indexValue) pivotRow
    
reduceRows :: (A.Array a, A.Array b, F.Field c) => Int -> a (b c) -> a (b c)
reduceRows index matrix = A.fromFunction rowAt
  where
    rowAt i = if i <= index
              then A.get i matrix
              else reduceRow i index matrix

rowEchelonRow :: (A.Array a, A.Array b, F.Field c, Ord c) => Int -> a (b c) -> Maybe (a (b c))
rowEchelonRow index matrix = fmap (reduceRows index) (fmap relocatePivot (findPivot index matrix))
  where
    relocatePivot pivot = A.swap pivot index matrix

rowEchelonRows :: (A.Array a, A.Array b, F.Field c, Ord c) => Int -> a (b c) -> Maybe (a (b c))
rowEchelonRows index matrix =
  let
    dim = A.size matrix
  in
    if index == dim
    then Just matrix
    else rowEchelonRow index matrix >>= rowEchelonRows (index + 1)

rowEchelon :: (A.Array a, A.Array b, F.Field c, Ord c) => a (b c) -> Maybe (a (b c))
rowEchelon matrix = rowEchelonRows 0 matrix

{-
solve :: (A.Array a, A.Array b, F.Field c, Ord c) => a (b c) -> Maybe (a c)
solve m = pure (solveRows 0 (pure AG.identity)) <*> rowEchelon m

solveRows :: (A.Array a, A.Array b, F.Field c) => Int -> a c -> a (b c) -> a c
solveRows i solution matrix =
  let
    (rows, cols) = size matrix
  in
    if i == rows
    then solution
    else solveRows (i + 1) (solveRow i solution matrix) matrix

solveRow :: (A.Array a, A.Array b, F.Field c) => Int -> a c -> a (b c) -> a c
solveRow index solution matrix = A.set index solution
  where
    index' = rows - index - 1
    (rows, cols) = size matrix
    d = get (index', rows) matrix
    value = AG.invert (AG.add d (valueFromRow index'))
    valueFromRow i =
      if i == rows
      then AG.identity
      else AG.add (get (index', i) matrix) * (A.get i solution)
-}

testMatrix :: Test
testMatrix = testSuite "Matrix" [
  testDeterminant,
  testFindPivot,
  testReduceRow,
  testReduceRows,
  testRowEchelonRow,
  testRowEchelonZeroColumn,
  testRowEchelon,
  testRowEchelonInvalid
  ]

testDeterminant :: Test
testDeterminant = testSuite "determinant" [testDeterminant1, testDeterminant2, testDeterminant3, testDeterminant4]

testDeterminant1 :: Test
testDeterminant1 = test "determinant 1x1" (assertEquals expected (determinant matrix))
  where
    expected :: Float
    expected = 3
    matrix :: Matrix1
    matrix = matrix1 3

testDeterminant2 :: Test
testDeterminant2 = test "determinant 2x2" (assertEquals expected (determinant matrix))
  where
    expected :: Float
    expected = 3 * 5 - 2 * 7
    matrix :: Matrix2
    matrix = matrix2 3 2 7 5

testDeterminant3 :: Test
testDeterminant3 = test "determinant 3x3" (assertEquals expected (determinant matrix))
  where
    expected :: Float
    expected = -306
    matrix :: Matrix3
    matrix = matrix3
              6 1 1
              4 (-2) 5
              2 8 7
              
testDeterminant4 :: Test
testDeterminant4 = test "determinant 4x4" (assertEquals expected (determinant matrix))
  where
    expected :: Float
    expected = -39
    matrix :: Matrix4
    matrix = matrix4
             1 2 (-1) 3
             2 1 (-2) 3
             3 1 2 1
             1 (-1) 0 2
             
testFindPivot :: Test
testFindPivot = test "Find pivot" (assertEquals (Just 1) pivot1 >>
                                   assertEquals (Just 1) pivot2 >>
                                   assertEquals Nothing pivot3)
  where
    m :: A.Array2 (A.Array2 Float)
    m = matrix2 1.0 4.0 2.0 1.0
    pivot1 = findPivot 0 m
    m2 :: A.Array3 (A.Array3 Float)
    m2 = matrix3 1.0 4.0 2.0 0.0 5.0 2.0 0.0 3.0 1.0
    pivot2 = findPivot 1 m2
    m3 :: A.Array2 (A.Array2 Float)
    m3 = matrix2 0.0 4.0 0.0 1.0
    pivot3 = findPivot 0 m3

testReduceRow :: Test
testReduceRow = test "Reduce row" (assertEquals expected actual)
  where
    expected :: A.Array3 Float
    expected = A.array3 0.0 (2.0 * 3.0 - 4.0 * 1.0) (2.0 * 3.0 - 4.0 * 4.0)
    actual :: A.Array3 Float
    actual = reduceRow 1 0 (matrix2x3
                             2.0 1.0 4.0
                             4.0 3.0 3.0
                           )

testReduceRows :: Test
testReduceRows = test "Reduce rows" (assertEquals expected actual)
  where
    expected :: A.Array3 (A.Array4 Float)
    expected = matrix3x4
               4.0 3.0 3.0 2.0
               0 (-2.0) 10.0 4.0
               0 5.0 21.0 14.0
               
    actual :: A.Array3 (A.Array4 Float)
    actual = reduceRows 0 (matrix3x4
                             4.0 3.0 3.0 2.0
                             2.0 1.0 4.0 2.0 
                             1.0 2.0 6.0 4.0
                           )

testRowEchelonRow :: Test
testRowEchelonRow = test "Triangulate row" (assertEquals expected actual)
  where
    expected :: Maybe (A.Array3 (A.Array4 Float))
    expected = Just (matrix3x4
                     4.0 3.0 3.0 2.0
                     0 (-2.0) 10.0 4.0
                     0 5.0 21.0 14.0
                    )
               
    actual :: Maybe (A.Array3 (A.Array4 Float))
    actual = rowEchelonRow 0 (matrix3x4
                             2.0 1.0 4.0 2.0 
                             4.0 3.0 3.0 2.0 
                             1.0 2.0 6.0 4.0
                           )

testRowEchelonZeroColumn :: Test
testRowEchelonZeroColumn = test "Row echelon zero column" (assertEquals expected actual)
  where
    expected :: Maybe (A.Array3 (A.Array4 Float))
    expected = Nothing
               
    actual :: Maybe (A.Array3 (A.Array4 Float))
    actual = rowEchelonRow 0 (matrix3x4
                             0.0 3.0 3.0 2.0
                             0.0 1.0 4.0 2.0 
                             0.0 2.0 6.0 4.0
                           )

testRowEchelon :: Test
testRowEchelon = test "Row echelon" (assertEquals expected actual)
 where
    expected :: Maybe (A.Array3 (A.Array4 Float))
    expected = Just (matrix3x4
                      4 3 3 2
                      0 5 21 14
                      0 0 92 48
                    )
               
    actual :: Maybe (A.Array3 (A.Array4 Float))
    actual = rowEchelon (matrix3x4
                           2.0 1.0 4.0 2.0 
                           4.0 3.0 3.0 2.0 
                           1.0 2.0 6.0 4.0
                         )

testRowEchelonInvalid :: Test
testRowEchelonInvalid = test "Row echelon invalid" (assertEquals expected actual)
 where
    expected :: Maybe (A.Array3 (A.Array4 Float))
    expected = Nothing             
    actual :: Maybe (A.Array3 (A.Array4 Float))
    actual = rowEchelon (matrix3x4
                           2.0 1.0 4.0 2.0 
                           4.0 3.0 3.0 2.0 
                           6.0 4.0 7.0 4.0
                         )
