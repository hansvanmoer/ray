module Vector (dot, getA, getX, getY, getZ, norm, normSquared, scale, testVector, vector1, vector2, vector3, Vector1, Vector2, Vector3, Vector4) where

import Data.Maybe
import qualified Array as A
import qualified AdditiveGroup as AG
import qualified Continuum as C
import qualified Field as F
import Test

type Vector1 = A.Array1 Float

type Vector2 = A.Array2 Float

type Vector3 = A.Array3 Float

type Vector4 = A.Array4 Float

vector1 :: (F.Field a) => a -> A.Array1 a
vector1 = A.array1

vector2 :: (F.Field a) => a -> a -> A.Array2 a
vector2 = A.array2

vector3 :: (F.Field a) => a -> a -> a -> A.Array3 a
vector3 = A.array3

vector4 :: (F.Field a) => a -> a -> a -> a -> A.Array4 a
vector4 = A.array4

getX :: (A.Array a) => a b -> b
getX = A.get 0

getY :: (A.Array a) => a b -> b
getY = A.get 1

getZ :: (A.Array a) => a b -> b
getZ = A.get 2

getA :: (A.Array a) => a b -> b
getA = A.get 3


scale :: (Functor a, F.Field b) => b -> a b -> a b
scale l r = fmap (F.multiply l) r

dot :: (Applicative a, Foldable a, F.Field b) => a b -> a b -> b
dot l r = foldr AG.add AG.identity (pure F.multiply <*> l <*> r)

normSquared :: (Applicative a, Foldable a, F.Field b) => a b -> b
normSquared v = dot v v

norm :: (Applicative a, Foldable a, C.Continuum b) => a b -> b
norm v = fromJust (C.squareRoot (normSquared v))

testVector :: Test
testVector = testSuite "Vector"
  [
    testScale,
    testDot,
    testNormSquared,
    testNorm
  ]

testScale :: Test
testScale = test "Scale" (assertEquals expected actual)
  where
    expected :: A.Array3 Float
    expected = vector3 0.0 2.0 4.0
    actual :: A.Array3 Float
    actual = scale 2.0 (vector3 0.0 1.0 2.0)

testDot :: Test
testDot = test "Dot product" (assertEquals expected actual)
  where
    expected :: Float
    expected = 0.0 * 3.0 + 1.0 * 4.0 + 2.0 * 5.0
    actual :: Float
    actual = dot (vector3 0.0 1.0 2.0) (vector3 3.0 4.0 5.0)

testNormSquared :: Test
testNormSquared = test "Norm squared" (assertEquals expected actual)
  where
    expected :: Float
    expected = 25.0
    actual :: Float
    actual = normSquared (vector3 0.0 3.0 4.0)

testNorm :: Test
testNorm = test "Norm" (assertEquals expected actual)
  where
    expected :: Float
    expected = 5.0
    actual :: Float
    actual = norm (vector3 0.0 3.0 4.0)
