module Main where

import Classify
import Test.HUnit
 
testZero = TestCase $ assertEqual  "a sphere" "Zero" $ normalize "(abc)+(a'c'b')"
testCrosscap = TestCase $ assertEqual  "1 crosscap" "Crosscaps 1" $ normalize "(ab)+(b'a)"
testHandle = TestCase $ assertEqual  "1 handle" "Handles 1" $ normalize "(ca)+(b'abc)"
test2Crosscaps = TestCase $ assertEqual  "A crosshandle" "Crosscaps 2" $ normalize "(aba'b)"
test3Crosscaps = TestCase $
                 assertEqual  "1 crosscap + 1 handle" "Crosscaps 3" $ normalize "(cadd)+(b'abc)"
test4Crosscaps = TestCase $
                 assertEqual  "2 crosscaps + 1 handle" "Crosscaps 4" $ normalize "(aba'c)(d)+(d'bc)"
testZipHoles = TestCase $ assertEqual  "invert bc" "Crosscaps 2" $ normalize "(abc)(cab)"

unitTests = TestList [TestLabel "zero" testZero,
                      TestLabel "handle" testHandle,
                      TestLabel "crosscap" testCrosscap,
                      TestLabel "2 crosscaps" test2Crosscaps,
                      TestLabel "3 crosscaps" test3Crosscaps,
                      TestLabel "4 crosscaps" test4Crosscaps,
                      TestLabel "zip same direction holes" testZipHoles]
 
main :: IO ()
main = do _ <- runTestTT unitTests
          return ()
