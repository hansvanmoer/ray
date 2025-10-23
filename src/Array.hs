module Array (Array, fromFunction, get, size, Array1, array1, Array2, array2, Array3, array3, Array4, array4, set, swap, indexOf, indexOfFrom, scan, scanFrom, testArray) where

import Test

class (Applicative a, Foldable a, Functor a, Monad a) => Array a where
  fromFunction :: (Int -> b) -> a b
  get :: Int -> a b -> b
  size :: a b -> Int

set :: (Array a) => Int -> b -> a b -> a b
set index value array = fromFunction valueAt
  where
    valueAt x = if x == index
                then value
                else get x array

swap :: (Array a) => Int -> Int -> a b ->  a b
swap i j array = if i >= 0 && i < dim && j >= 0 && j < dim && i /= j
                 then fromFunction valueAt
                 else array
  where
    dim = size array
    valueAt x = if x == i
                then get j array
                else if x == j
                     then get i array
                     else get x array

indexOf :: (Array a) => (b -> Bool) -> a b -> Maybe Int
indexOf pred array = indexOfFrom 0 pred array

indexOfFrom :: (Array a) => Int -> (b -> Bool) -> a b -> Maybe Int
indexOfFrom start pred array = indexOfFrom' start
  where
    dim = size array
    indexOfFrom' i = if i >= dim
                     then Nothing
                     else if pred (get i array)
                          then Just i
                          else indexOfFrom' (i + 1)

scanFrom :: (Array a) => Int -> (b -> b -> Bool) -> b -> a b -> Maybe Int
scanFrom start pred i array = if result == dim
                              then Nothing
                              else Just result
  where
    result = scanFrom' start dim i
    dim = size array
    scanFrom' j index value = if j == dim
                              then index
                              else
                                let
                                  cur = get j array
                                in
                                  if pred value cur
                                  then scanFrom' (j + 1) j cur
                                  else scanFrom' (j + 1) index value

scan :: (Array a) => (b -> b -> Bool) -> b -> a b -> Maybe Int
scan pred i array = scanFrom 0 pred i array

data Array1 a = A1 a

array1 :: a -> Array1 a
array1 = A1

instance Array Array1 where
  fromFunction f = A1 (f 0)
  get _ (A1 e0) = e0
  size _ = 1

instance (Eq a) => Eq (Array1 a) where
  (==) (A1 l0) (A1 r0) = l0 == r0

instance (Show a) => Show (Array1 a) where
  show (A1 e0) = "[" ++ show e0 ++ "]"

instance Functor Array1 where
  fmap f (A1 e0) = A1 (f e0)

instance Applicative Array1 where
  pure = A1
  (<*>) (A1 l0) (A1 r0) = A1 (l0 r0)

instance Monad Array1 where
  return = pure
  (>>=) (A1 e0) f =
    let
      (A1 f0) = f e0
    in
      A1 f0

instance Foldable Array1 where
  foldr accum i (A1 e0) = accum e0 i

data Array2 a = A2 a a

array2 :: a -> a -> Array2 a
array2 = A2

instance Array Array2 where
  fromFunction f = A2 (f 0) (f 1)
  get 0 (A2 e0 _) = e0
  get _ (A2 _ e1) = e1
  size _ = 2

instance (Eq a) => Eq (Array2 a) where
  (==) (A2 l0 l1) (A2 r0 r1) = l0 == r0 && l1 == r1

instance (Show a) => Show (Array2 a) where
  show (A2 e0 e1) = "[" ++ show e0 ++ ", " ++ show e1 ++ "]"

instance Functor Array2 where
  fmap f (A2 e0 e1) = A2 (f e0) (f e1)

instance Applicative Array2 where
  pure v = A2 v v
  (<*>) (A2 l0 l1) (A2 r0 r1) = A2 (l0 r0) (l1 r1)

instance Monad Array2 where
  return = pure
  (>>=) (A2 e0 e1) f =
    let
      (A2 f0 _) = f e0
      (A2 _ f1) = f e1
    in
      A2 f0 f1

instance Foldable Array2 where
  foldr accum i (A2 e0 e1) = accum e1 $ accum e0 i

data Array3 a = A3 a a a

array3 :: a -> a -> a -> Array3 a
array3 = A3

instance Array Array3 where
  fromFunction f = A3 (f 0) (f 1) (f 2)
  get 0 (A3 e0 _ _) = e0
  get 1 (A3 _ e1 _) = e1
  get _ (A3 _ _ e2) = e2
  size _ = 3

instance (Eq a) => Eq (Array3 a) where
  (==) (A3 l0 l1 l2) (A3 r0 r1 r2) = l0 == r0 && l1 == r1 && l2 == r2

instance (Show a) => Show (Array3 a) where
  show (A3 e0 e1 e2) = "[" ++ show e0 ++", " ++ show e1 ++ ", " ++ show e2 ++ "]"

instance Functor Array3 where
  fmap f (A3 e0 e1 e2) = A3 (f e0) (f e1) (f e2)

instance Applicative Array3 where
  pure v = A3 v v v
  (<*>) (A3 l0 l1 l2) (A3 r0 r1 r2) = A3 (l0 r0) (l1 r1) (l2 r2)

instance Monad Array3 where
  return = pure
  (>>=) (A3 e0 e1 e2) f =
    let
      (A3 f0 _ _) = f e0
      (A3 _ f1 _) = f e1
      (A3 _ _ f2) = f e2
    in
      A3 f0 f1 f2

instance Foldable Array3 where
  foldr accum i (A3 e0 e1 e2) = accum e2 $ accum e1 $ accum e0 i

data Array4 a = A4 a a a a

array4 :: a -> a -> a -> a -> Array4 a
array4 = A4

instance Array Array4 where
  fromFunction f = A4 (f 0) (f 1) (f 2) (f 3)
  get 0 (A4 e0 _ _ _) = e0
  get 1 (A4 _ e1 _ _) = e1
  get 2 (A4 _ _ e2 _) = e2
  get _ (A4 _ _ _ e3) = e3
  size _ = 4


instance (Eq a) => Eq (Array4 a) where
  (==) (A4 l0 l1 l2 l3) (A4 r0 r1 r2 r3) = l0 == r0 && l1 == r1 && l2 == r2 && l3 == r3

instance (Show a) => Show (Array4 a) where
  show (A4 e0 e1 e2 e3) = "[" ++ show e0 ++", " ++ show e1 ++ ", " ++ show e2 ++ ", " ++ show e3 ++ "]"

instance Functor Array4 where
  fmap f (A4 e0 e1 e2 e3) = A4 (f e0) (f e1) (f e2) (f e3)

instance Applicative Array4 where
  pure v = A4 v v v v
  (<*>) (A4 l0 l1 l2 l3) (A4 r0 r1 r2 r3) = A4 (l0 r0) (l1 r1) (l2 r2) (l3 r3)

instance Monad Array4 where
  return = pure
  (>>=) (A4 e0 e1 e2 e3) f =
    let
      (A4 f0 _ _ _) = f e0
      (A4 _ f1 _ _) = f e1
      (A4 _ _ f2 _) = f e2
      (A4 _ _ _ f3) = f e3
    in
      A4 f0 f1 f2 f3

instance Foldable Array4 where
  foldr accum i (A4 e0 e1 e2 e3) = accum e3 $ accum e2 $ accum e1 $ accum e0 i

initTestArray1 :: (Int -> Int) -> Array1 Int
initTestArray1 = fromFunction

initTestArray2 :: (Int -> Int) -> Array2 Int
initTestArray2 = fromFunction

initTestArray3 :: (Int -> Int) -> Array3 Int
initTestArray3 = fromFunction

initTestArray4 :: (Int -> Int) -> Array4 Int
initTestArray4 = fromFunction

type TestArrayConstructor a = (Int -> Int) -> a Int

testArray :: Test
testArray = testSuite "Array"
            (
              specificTests ++
              (testArrayFunctions initTestArray1) ++
              (testArrayFunctions initTestArray2) ++
              (testArrayFunctions initTestArray3) ++
              (testArrayFunctions initTestArray4)
            )
  where
    specificTests =
      [
        testFromFunction (array1 0),
        testFromFunction (array2 0 2),
        testFromFunction (array3 0 2 4),
        testFromFunction (array4 0 2 4 6)
      ]

testArrayFunctions :: (Array a, Eq (a Int), Show (a Int)) => TestArrayConstructor a -> [Test]
testArrayFunctions init = fmap (\x -> x init) [
  testEq,
  testShow,
  testFMap,
  testPure,
  testFunctorIdentity,
  testFunctorConcat,
  testApply,
  testApplicativeIdentity,
  testApplicativeHomomorphism,
  testApplicativeInterchange,
  testApplicativeComposition,
  testReturn,
  testBind,
  testMonadLeftIdentity,
  testMonadRightIdentity,
  testMonadAssociativity,
  testFoldable,
  testGet,
  testSet,
  testSwap,
  testIndexOf,
  testIndexOfFrom
  ]

arrayTestName :: Int -> String -> String
arrayTestName dim functionName = "Array" ++ show dim ++ " " ++ functionName 

arrayInitTestFunction :: Int -> Int
arrayInitTestFunction i = i * 2

testFromFunction :: (Array a, Eq (a Int), Show (a Int)) => a Int -> Test
testFromFunction array = test (arrayTestName dim "fromFunction") (assertEquals array (fromFunction arrayInitTestFunction))
  where
    dim = size array

testShow :: (Array a, Show (a Int)) => TestArrayConstructor a -> Test
testShow init = test (arrayTestName dim "show") (assertEquals expected (show value))
  where
    func :: Int -> Int
    func i = i * 2
    value = init func
    dim = size value
    expected = "[" ++ (foldr concat "]" ((fmap show [func i | i <- [0..(dim - 1)]])))
    concat x y = if y == "]" then x ++ y else x ++", " ++ y

testEq :: (Array a, Show (a Int), Eq (a Int)) => TestArrayConstructor a -> Test
testEq init = test (arrayTestName dim "eq") (assertEquals array array)
  where
    array = init (\i -> i * 2) 
    dim = size array

testFMap :: (Array a, Show (a Int), Eq (a Int)) => TestArrayConstructor a -> Test
testFMap init = test (arrayTestName dim "fmap") (assertEquals expected (fmap (*2) array))
  where
    array = init (\i -> i * 2)
    dim = size array
    expected = init (\i -> i * 4)

testFunctorIdentity :: (Array a, Show (a Int), Eq (a Int)) => TestArrayConstructor a -> Test
testFunctorIdentity init = test (arrayTestName dim "functor identity law") (assertEquals (id array) (fmap id array))
  where
    array = init (\i -> i * 2)
    dim = size array

testFunctorConcat :: (Array a, Show (a Int), Eq (a Int)) => TestArrayConstructor a -> Test
testFunctorConcat init = test (arrayTestName dim "functor concatenation law") (assertEquals (fmap (f . g) array) (((fmap f) . (fmap g)) array))
  where
    array = init (\i -> i * 2)
    f = (*3)
    g = (*4)
    dim = size array

testPure :: (Array a, Show (a Int), Eq (a Int)) => TestArrayConstructor a -> Test
testPure init = test (arrayTestName dim "pure") (assertEquals array (pure 1))
  where
    array = init (\i -> 1)
    dim = size array

testApply :: (Array a, Show (a Int), Eq (a Int)) => TestArrayConstructor a -> Test
testApply constructor = test (arrayTestName dim "apply") (assertEquals expected (pure (+) <*> pure 1 <*> constructor (\i -> i * 2)))
  where
    expected = constructor (\i -> 1 + i * 2)
    dim = size expected

testApplicativeIdentity :: (Array a, Show (a Int), Eq (a Int)) => TestArrayConstructor a -> Test
testApplicativeIdentity init = test (arrayTestName dim "applicative identity") (assertEquals array (pure id <*> array))
  where
    array = init (\i -> i * 2)
    dim = size array

testApplicativeHomomorphism :: (Array a, Show (a Int), Eq (a Int)) => TestArrayConstructor a -> Test
testApplicativeHomomorphism init = test (arrayTestName dim "applicative homomorphism")
  (assertAll [
      (assertEquals array (pure f <*> pure x)),
      (assertEquals array (pure (f x)))
   ])
  where
    array = init (\_ -> 6)
    x = 2
    f = (*3)
    dim = size array

testApplicativeInterchange :: (Array a, Show (a Int), Eq (a Int)) => TestArrayConstructor a -> Test
testApplicativeInterchange init = test (arrayTestName dim "applicative interchange")
  (assertAll [
      (assertEquals array (pure f <*> pure x)),
      (assertEquals array (pure ($ x) <*> pure f))
   ])
  where
    array = init (\_ -> 6)
    x = 2
    f = (*3)
    dim = size array
    
testApplicativeComposition :: (Array a, Show (a Int), Eq (a Int)) => TestArrayConstructor a -> Test
testApplicativeComposition init = test (arrayTestName dim "applicative associativity")
  (assertAll [
      (assertEquals array (pure (.) <*> pure f <*> pure g <*> pure h)),
      (assertEquals array (pure f <*> (pure g <*> pure h)))
   ])
  where
    array = init (\_ -> 144)
    f = (*3)
    g = (*4)
    h = 12
    dim = size array

testReturn :: (Array a, Show (a Int), Eq (a Int)) => TestArrayConstructor a -> Test
testReturn init = test (arrayTestName dim "return") (assertEquals array (return 1))
  where
    array = init (\_ -> 1)
    dim = size array

testBind :: (Array a, Show (a Int), Eq (a Int)) => TestArrayConstructor a -> Test
testBind init = test (arrayTestName dim "bind") (assertEquals expected (array >>= f))
  where
    array = init (\i -> i)
    f i = init (\j -> j * i)
    expected = init (\i -> i * i) 
    dim = size array

testMonadLeftIdentity :: (Array a, Show (a Int), Eq (a Int)) => TestArrayConstructor a -> Test
testMonadLeftIdentity init = test (arrayTestName dim "monad left identity") (assertAll [first, second])
  where
    first = assertEquals expected (return x >>= f)
    second = assertEquals expected (f x)
    x = 2
    f i = init (\j -> j * i)
    expected = init (\i -> i * 2)
    dim = size expected
    
testMonadRightIdentity :: (Array a, Show (a Int), Eq (a Int)) => TestArrayConstructor a -> Test
testMonadRightIdentity init = test (arrayTestName dim "monad right identity") (assertEquals expected (expected >>= return))
  where
    expected = init (\i -> i * 2)
    dim = size expected

testMonadAssociativity :: (Array a, Show (a Int), Eq (a Int)) => TestArrayConstructor a -> Test
testMonadAssociativity init = test (arrayTestName dim "monad associativity") (assertAll [first, second])
  where
    first = assertEquals expected ((array >>= f) >>= g)
    second = assertEquals expected (array >>= (\x -> f x >>= g))
    f i = init (\j -> j * i)
    g i = init (\j -> j + i)
    array = init (\i -> i)
    expected = init (\i -> [0, 2, 6, 12] !! i)
    dim = size array

testFoldable :: (Array a, Show (a Int), Eq (a Int)) => TestArrayConstructor a -> Test
testFoldable init = test (arrayTestName dim "foldr") (assertEquals expected (foldr (+) 2 array))
  where
    array = init (\i -> i)
    dim = size array
    expected = foldr (+) 2 [0..(dim-1)]

testGet :: (Array a, Show (a Int), Eq (a Int)) => TestArrayConstructor a -> Test
testGet init = testSuite (arrayTestName dim "get") [testGetAt i init | i <- [0..4]]
  where
    dim = size array
    array = init id
    
testGetAt :: (Array a, Show (a Int), Eq (a Int)) => Int -> TestArrayConstructor a -> Test
testGetAt index init = test (arrayTestName dim ("get " ++ show index)) (assertEquals expected (get index array))
  where
    dim = size array
    array = init (*2)
    expected = if index < dim then index * 2 else (dim - 1) * 2

testSet :: (Array a, Show (a Int), Eq (a Int)) => TestArrayConstructor a -> Test
testSet init = testSuite (arrayTestName dim "set") [testSetAt i init | i <- [0..4]]
  where
    dim = size array
    array = init id

testSetAt :: (Array a, Show (a Int), Eq (a Int)) => Int -> TestArrayConstructor a -> Test
testSetAt index init = test (arrayTestName dim ("set " ++ show index)) (assertEquals expected modified)
  where
    dim = size array
    array = init (*2)
    value = dim * 10
    modified = set index value array
    expected = init valueAt
    valueAt i = if i < dim
                then if i == index
                     then value
                     else i * 2
                else (dim - 1) * 2

testSwap :: (Array a, Show (a Int), Eq (a Int)) => TestArrayConstructor a -> Test
testSwap init = testSuite (arrayTestName dim "swap") [testSwapAt i j init | i <- [0..4], j <- [0..4]]
  where
    dim = size array
    array = init id

testSwapAt :: (Array a, Show (a Int), Eq (a Int)) => Int -> Int -> TestArrayConstructor a -> Test
testSwapAt i j init = test (arrayTestName dim ("swap (" ++ show i ++ ", " ++ show j ++ ")")) (assertEquals expected modified)
  where
    dim = size array
    array = init (*2)
    modified = swap i j array
    expected = init valueAt
    valueAt d = if i < dim && j < dim
                then if d < dim
                     then if d == i
                          then j * 2
                          else if d == j
                               then i * 2
                               else d * 2
                     else (dim - 1) * 2
                else if d < dim
                     then d * 2
                     else (dim - 1) * 2

testIndexOf :: (Array a, Show (a Int), Eq (a Int)) => TestArrayConstructor a -> Test
testIndexOf init = testSuite (arrayTestName dim "indexOf") [testIndexOfAt i init | i <- [0..4]]
  where
    dim = size array
    array = init id

testIndexOfAt :: (Array a, Show (a Int), Eq (a Int)) => Int -> TestArrayConstructor a -> Test
testIndexOfAt index init = test (arrayTestName dim ("indexOf " ++ show index)) (assertEquals expected (indexOf pred array))
  where
    dim = size array
    array = init (*2)
    pred i = i == index * 2
    expected = if index < dim then Just index else Nothing

testIndexOfFrom :: (Array a, Show (a Int), Eq (a Int)) => TestArrayConstructor a -> Test
testIndexOfFrom init = testSuite (arrayTestName dim "indexOf from") [testIndexOfFromAt i j init | i <- [0..4], j <- [0..4]]
  where
    dim = size array
    array = init id

testIndexOfFromAt :: (Array a, Show (a Int), Eq (a Int)) => Int -> Int -> TestArrayConstructor a -> Test
testIndexOfFromAt index from init = test (arrayTestName dim ("indexOf " ++ show index ++ " from " ++ show from)) (assertEquals expected (indexOfFrom from pred array))
  where
    dim = size array
    array = init (*2)
    pred i = i == index * 2
    expected = if index < from
               then Nothing
               else if index < dim
                    then Just index
                    else Nothing
