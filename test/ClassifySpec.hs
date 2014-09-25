module Main where

import Data.List

dup l = concat $ [[x,x] | x <- l]

main :: IO ()
main = return ()  -- todo
