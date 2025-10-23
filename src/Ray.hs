module Ray (Ray, ray, getStart, getDirection, testRay) where

import Test
import qualified AdditiveGroup as AG
import qualified Array as A
import qualified Field as F
import qualified Vector as V

data Ray = R V.Vector3 V.Vector3

ray :: V.Vector3 -> V.Vector3 -> Ray
ray = R

getStart :: Ray -> V.Vector3
getStart (R s _) = s

getDirection :: Ray -> V.Vector3
getDirection (R _ d) = d

instance Eq Ray where
  (==) (R lp ld) (R rp rd) = lp == rp && isSameDirection ld rd

isSameDirection :: (A.Array a, F.Field b, Eq b, Ord b) => a b -> a b -> Bool
isSameDirection l r = (d == Nothing || d >= Just AG.identity) && isSameDirectionAt 0
  where
    d = F.divide (A.get 0 l) (A.get 0 r)
    dim = A.size l
    isSameDirectionAt i = if i == dim
                          then True
                          else
                            if d == ((F.divide (A.get i l) (A.get i r)))
                            then isSameDirectionAt (i + 1)
                            else False
      
instance Show Ray where
  show (R p d) = "{" ++ show p ++ " + k " ++ show d ++ "}"

testRay :: Test
testRay = testSuite "Ray" [testStart, testDirection, testRayEquals, testRayEqualsScaled, testRayNotEqualsWrongDirection]

testStart :: Test
testStart = test "Ray start" (assertEquals expected (getStart r))
  where
    expected :: V.Vector3
    expected = V.vector3 1 2 3
    r = ray expected (V.vector3 2 2 2)

testDirection :: Test
testDirection = test "Ray direction" (assertEquals expected (getDirection r))
  where
    expected :: V.Vector3
    expected = V.vector3 1 2 3
    vector = expected
    r = ray (V.vector3 2 2 2) expected

testRayEquals :: Test
testRayEquals = test "Ray equals" (assertEquals expected value)
  where
    expected = ray (V.vector3 1 2 3) (V.vector3 2 5 5)
    value = expected

testRayEqualsScaled :: Test
testRayEqualsScaled = test "Ray equals scaled" (assertEquals expected value)
  where
    expected = ray (V.vector3 1 2 3) (V.vector3 2 5 5)
    value = ray (V.vector3 1 2 3) (V.vector3 4 10 10)

testRayNotEqualsWrongDirection :: Test
testRayNotEqualsWrongDirection = test "Ray does not equal wrong direction" (assertFalse (expected == value))
  where
    expected = ray (V.vector3 1 2 3) (V.vector3 2 5 5)
    value = ray (V.vector3 1 2 3) (V.vector3 (-2) (-5) (-5))
