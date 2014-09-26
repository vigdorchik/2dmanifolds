module Main where

import Classify
import Test.HUnit  -- QuickCheck in the future when possible.
 
testPlusSphere = TestCase $ assertEqual  "+ sphere" "(aa')+()" $ normalize "(aa')+()"
testSpherePlus = TestCase $ assertEqual  "sphere +" "()+(aa')" $ normalize "()+(aa')"
test2Spheres = TestCase $ assertEqual  "2 spheres" "()+()" $ normalize "()+()"
testSimple = TestCase $ assertEqual  "simple case" "(adc'a'c)(d)" $ normalize "(aba'c)(d)+(d'bc)"

unitTests = TestList [TestLabel "simple" testSimple,
                      TestLabel "2 spheres" test2Spheres,
                      TestLabel "sphere+" testSpherePlus,
                      TestLabel "+sphere" testPlusSphere]
 
main :: IO ()
main = do _ <- runTestTT unitTests
          return ()

