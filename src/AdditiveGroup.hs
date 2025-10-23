{-# LANGUAGE FlexibleInstances #-}

module AdditiveGroup (AdditiveGroup, add, identity, invert, subtract, absolute, testAdditiveGroup) where

import Test
import Prelude hiding (subtract)

-- An additive group
class AdditiveGroup a where

  -- adds two values, producing a third
  add :: a -> a -> a

  -- The identity under addition
  identity :: a

  -- Inverts a value
  invert :: a -> a

  -- Subtracts a value
  subtract :: a -> a -> a
  subtract l r = add l (invert r)

instance AdditiveGroup Float where
  add = (+)
  identity = 0.0
  invert v = - v
  subtract = (-)

instance (Applicative a, AdditiveGroup b) => AdditiveGroup (a b) where
  add l r = pure add <*> l <*> r
  identity = pure identity
  invert v = fmap invert v
  subtract l r = pure subtract <*> l <*> r

absolute :: (AdditiveGroup a, Ord a) => a -> a
absolute value = if identity < value then value else invert value

testAdditiveGroup :: Test
testAdditiveGroup = testSuite "ApplicativeGroup" [testFloat, testApplicative]

testFloat :: Test
testFloat = testInstance "Float" value 3.0 (-2.0) 1.0 2.0 1.0
  where
    value :: Float
    value = 0.0

data TestApplicative a = TA a a

instance Functor TestApplicative where
  fmap f (TA x y) = TA (f x) (f y)

instance Applicative TestApplicative where
  pure v = TA v v
  (<*>) (TA lx ly) (TA rx ry) = TA (lx rx) (ly ry)

instance (Eq a) => Eq (TestApplicative a) where
  (==) (TA lx ly) (TA rx ry) = lx == rx && ly == ry

instance (Show a) => Show (TestApplicative a) where
  show (TA x y) = "<" ++ show x ++ ", " ++ show y ++ ">"

testApplicative :: Test
testApplicative = testInstance "Applicative" value (TA 3.0 6.0) (TA (-2.0) (-4.0)) (TA 1.0 2.0) (TA 2.0 4.0) (TA 1.0 2.0)
  where
    value :: TestApplicative Float
    value = TA 0.0 0.0

testInstance :: (AdditiveGroup a, Eq a, Show a) => String -> a -> a -> a -> a -> a -> a -> Test
testInstance typeName zero sum inverted subtracted l r = testSuite testName
  [
    test "identity" (assertEquals zero identity),
    test "add" (assertEquals sum (add l r)),
    test "invert" (assertEquals inverted (invert l)),
    test "subtract" (assertEquals subtracted (subtract l r))
  ]
  where
    testName = "AdditiveGroup " ++ typeName
