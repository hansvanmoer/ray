module Main where

import AdditiveGroup
import Array
import Json
import Test
import Matrix
import Parser
import Ray
import Vector

main :: IO ()
main = runTests >> return ()

runTests :: IO Bool
runTests = runTest (testSuite "Tests" [testAdditiveGroup, testArray, testJson, testMatrix, testParser, testRay, testVector])


