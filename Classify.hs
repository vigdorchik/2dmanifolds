module Classify (normalize) where

import Data.List
import Control.Monad
import Control.Arrow
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Prim (parse)

data Edge = Dir Char | Inv Char

edgeLabel (Dir c) = c
edgeLabel (Inv c) = c

newtype Hole = Hole {
  edges :: [Edge]
}

instance Show Hole where
  show = concat . map showEdge . edges where
    showEdge (Dir c) = show c
    showEdge (Inv c) = [c, '\'']

newtype Sphere = Sphere {
  holes :: [Hole]
}

instance Show Sphere where
  show = unwords . map ((:) '(' . flip (++) ")" . show) . holes

newtype Surface = Surface {
  spheres :: [Sphere]
}

instance Show Surface where
  show = intercalate "+" . map show . spheres

invP = do c <- letter; _ <- char '\''; return c
edgeP = choice [fmap Inv invP, fmap Dir letter]
holeP = fmap Hole $ many1 edgeP
sphereP = fmap Sphere $ many1 $ between (char '(') (char ')') holeP
surfaceP = sepBy1 sphereP (char '+')
    
fromStr :: String -> Either String [Sphere]
fromStr s = left show $ parse surfaceP "" s
      
validate :: String -> Either String [Sphere]
validate = mfilter (\spheres' ->
                     let holes' = concat $ map holes spheres'
                         edges' = concat $ map edges holes'
                         labels' = map edgeLabel edges'
                         nub' = nub labels' in
                     nub' == (labels' \\ nub')) . fromStr
  
normalize :: String -> String
normalize s = case validate s of
  Left err -> error err
  Right (spheres) -> show $ zipSpheres spheres  -- todo

zipSpheres :: [Sphere] -> [Surface]
zipSpheres [s] = [Surface [s]] -- todo
