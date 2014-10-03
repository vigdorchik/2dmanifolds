module Main where

import Classify
import Test.HUnit  -- QuickCheck in the future when possible.
 
testCrosscap = TestCase $ assertEqual  "1 crosscap" "Crosscaps 1" $ normalize "(ab)+(b'a)"
testHandle = TestCase $ assertEqual  "1 handle" "Handles 1" $ normalize "(ca)+(b'abc)"
test3Crosscaps = TestCase $ assertEqual  "3 crosscaps" "Crosscaps 3" $ normalize "(cadd)+(b'abc)"
testSimple = TestCase $ assertEqual  "simple case" "(adc'a'c)(d)" $ normalize "(aba'c)(d)+(d'bc)"

unitTests = TestList [--TestLabel "simple" testSimple,
                      TestLabel "handle" testHandle,
                      TestLabel "crosscap" testCrosscap,
                      TestLabel "3 crosscaps" test3Crosscaps]
 
main :: IO ()
main = do _ <- runTestTT unitTests
          return ()

