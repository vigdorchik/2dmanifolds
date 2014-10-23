{-# LANGUAGE BangPatterns #-}
module Main where

import Classify
import Test.HUnit
import Test.QuickCheck
import Control.Monad
import Data.List
 
testZero = TestCase $ assertEqual  "a sphere" [Zero] $ normalize "(abc)+(a'c'b')"
testCrosscap = TestCase $ assertEqual  "1 crosscap" [Crosscaps 1] $ normalize "(ab)+(b'a)"
testHandle = TestCase $ assertEqual  "1 handle" [Handles 1] $ normalize "(ca)+(b'abc)"
test2Crosscaps = TestCase $ assertEqual  "A crosshandle" [Crosscaps 2] $ normalize "(aba'b)"
test3Crosscaps = TestCase $
                 assertEqual  "1 crosscap + 1 handle" [Crosscaps 3] $ normalize "(cadd)+(b'abc)"
test4Crosscaps = TestCase $ assertEqual  "2 crosscaps + 1 handle" [Crosscaps 4] $
                 normalize "(aba'c)(d)+(d'bc)"
testZipHoles = TestCase $ assertEqual  "invert bc" [Crosscaps 2] $ normalize "(abc)(cab)"

unitTests = TestList [TestLabel "zero" testZero,
                      TestLabel "handle" testHandle,
                      TestLabel "crosscap" testCrosscap,
                      TestLabel "2 crosscaps" test2Crosscaps,
                      TestLabel "3 crosscaps" test3Crosscaps,
                      TestLabel "4 crosscaps" test4Crosscaps,
                      TestLabel "zip same direction holes" testZipHoles]

-- just one sphere.
surfaceGen =
  do
    let num = 10
    n <- suchThat (fmap (flip mod num) arbitrary) ((<) 1)
    let l = take n ['a'..'z']
        fact :: Int -> Int
        fact 1 = 1
        fact !n = n * fact (n-1)
    f <- fmap (flip mod (fact (2*n))) arbitrary
    inv <- sequence $ replicate (2*n) arbitrary
    let permute [x] acc _ 1 = reverse (x:acc)
        permute l acc i lev  =
          let (d, m) = i `divMod` lev
              (pre, x:post) = splitAt m l in
          permute (pre++post) (x:acc) d (lev-1)
        p = permute (l++l) [] f (2*n)
        s = map (\(x, inv) -> if inv then [x, '\''] else [x]) (zip p inv)
        holed i l res = do
          j <- suchThat (fmap (flip mod n) arbitrary) (\j -> j > 0 && i+j <= 2*n)
          let (pre, post) = splitAt j l
              res' = if null res then pre else res ++ [")("] ++ pre
          if i+j == 2*n
            then return res' 
            else holed (i+j) post res'
    hs <- holed 0 s []
    return $ "(" ++ (concat hs) ++ ")"

card :: Canonical -> Int
card (Crosscaps n) = n
card (Handles n) = n*2
card Zero = 0

propLinear = forAll surfaceGen (\s ->
                                 let [normal] = normalize s in
                                 card normal <= (length $ filter (flip elem ['a'..'z']) s))

main :: IO ()
main = do _ <- runTestTT unitTests
          quickCheckWith stdArgs {maxSuccess=10000} propLinear
