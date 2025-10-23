module Test (Assertion, assertAll, assertEquals, assertFalse, assertTrue, Test, runTest, test, testSuite) where

type Assertion = IO Bool

assertAll :: [Assertion] -> Assertion
assertAll [] = return True
assertAll (x:xs) = x >>= (\r -> if r then assertAll xs else return False)

assertTrue :: Bool -> Assertion
assertTrue v = if v
               then return True
               else putStr "Assertion failed" >> return False

assertFalse :: Bool -> Assertion
assertFalse v = if v
                then putStr "Assertion failed" >> return False
                else return True

assertEquals :: (Eq a, Show a) => a -> a -> IO Bool
assertEquals expected actual =
  if expected == actual
  then
    return True
  else
    putStr ("Assertion failed: expected '" ++ show expected ++ "', but got '" ++ show actual ++ "'") >> return False

data Test = Assert String Assertion | Suite String [Test]

test :: String -> Assertion -> Test
test = Assert

testSuite :: String -> [Test] -> Test
testSuite = Suite

runTest :: Test -> IO Bool
runTest test = runTestWithPrefix "" test

runTestWithPrefix :: String -> Test -> IO Bool
runTestWithPrefix prefix (Assert name assertion) = putStr (prefix ++ "TEST '" ++ name ++ "' ") >> assertion >>= printResult
  where
    printResult result = putStr (if result then "-> SUCCESS\n" else " -> FAILURE\n") >> return result
runTestWithPrefix prefix (Suite name tests) = putStr (prefix ++ "TEST SUITE '" ++ name ++ "':\n") >> return True >>= runTestListWithPrefix (prefix ++ " ") tests >>= printResult
  where
    printResult result = putStr (prefix ++ (if result then "SUCCESS\n" else "FAILURE\n")) >> return result

runTestListWithPrefix :: String -> [Test] -> Bool -> IO Bool
runTestListWithPrefix prefix (x:xs) prevResult = runTestWithPrefix prefix x >>= (\result -> return (prevResult && result)) >>= runTestListWithPrefix prefix xs
runTestListWithPrefix prefix [] prevResult = return prevResult
