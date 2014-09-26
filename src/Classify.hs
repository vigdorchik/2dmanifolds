module Classify (normalize) where

import Classify.Cursor
import Data.List
import Control.Monad
import Control.Arrow (left)
import Control.Applicative ((<|>))
import Text.ParserCombinators.Parsec hiding ((<|>))
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Prim (parse)

data Edge = E Char | E_1 Char

edgeLabel (E c) = c
edgeLabel (E_1 c) = c

newtype Hole = Hole {
  edges :: [Edge]
}

instance Show Hole where
  show = concat . map showEdge . edges where
    showEdge (E c) = [c]
    showEdge (E_1 c) = [c, '\'']

newtype Sphere = Sphere {
  holes :: [Hole]
}

instance Show Sphere where
  show = concat . map ((:) '(' . flip (++) ")" . show) . holes

edgeP = do
  c <- letter
  option (E c) $ fmap (\_ -> E_1 c) (char '\'')
holeP = fmap Hole $ many edgeP
sphereP = fmap Sphere $ many1 $ between (char '(') (char ')') holeP
surfaceP = sepBy1 sphereP (char '+')
    
fromStr :: String -> Either String [Sphere]
fromStr s = left show $ parse surfaceP "" s
      
validate :: String -> Either String [Sphere]
validate = mfilter (\spheres' ->
                     let holes' = concatMap holes spheres'
                         edges' = concatMap edges holes'
                         labels' = sort $ map edgeLabel edges'
                         nub' = nub labels' in
                     nub' == (labels' \\ nub')) . fromStr
  
normalize :: String -> String
normalize s = case validate s of
  Left err -> error err
  Right spheres -> intercalate "+" (map normalizeSphere . zipSpheres $ spheres)

normalizeSphere :: Sphere -> String
normalizeSphere sphere =
  let alphabet = nub $ map edgeLabel . concatMap edges . holes $ sphere in
  show sphere -- todo

findJust :: [Maybe a] -> (Maybe a)
findJust = foldl' (<|>) Nothing

findTwin :: Edge -> [Sphere] -> Maybe (Cursor Edge, Cursor Hole, Cursor Sphere)
findTwin e =
  let l = edgeLabel e in
  findJust . (map (\cs@(_,s,_) -> findTwin' l cs (holes s))) . cursors where
    findTwin' l cs = findJust . (map (\ch@(_,h,_) -> findTwin'' l cs ch (edges h))) . cursors where
        findTwin'' l cs ch = fmap (\ce -> (ce, ch, cs)) .
                             find ((==) l . edgeLabel . pointed) . cursors

invert = reverse . (map inv) where
  inv (E u) = E_1 u
  inv (E_1 u) = E u

align (E _) (E _) es (pre,_,post) = (invert pre)++(invert post)++es
align (E_1 _) (E_1 _) es (pre,_,post) = (invert pre)++(invert post)++es
align _ _ es (pre,_,post) = post++pre++es

zipSpheres :: [Sphere] -> [Sphere]
zipSpheres = surfaces [] where
  surfaces acc [] = reverse acc
  surfaces acc (sphere:spheres) =
    let (surface, others) = zipOne (holes sphere) spheres in
    surfaces (surface:acc) others where
      zipOne holes' spheres =
        let doHoles [] doneHoles spheres = (Sphere $ reverse doneHoles, spheres)
            doHoles (h:tl) doneHoles spheres =
              let (hole, hs, ss) = doEdges (edges h) [] [] spheres in
              doHoles (hs++tl) (hole:doneHoles) ss where
                doEdges (e:tl) doneEdges newHoles spheres = case findTwin e spheres of
                  Nothing -> doEdges tl (e:doneEdges) newHoles spheres
                  Just (ce, ch, cs) ->
                    let ss = excluded cs
                        hs = (excluded ch)++newHoles
                        aligned = align e (pointed ce) tl ce in
                    doEdges aligned doneEdges hs ss
                doEdges [] doneEdges newHoles spheres =
                  (Hole $ reverse doneEdges, newHoles, spheres)
        in doHoles holes' [] spheres
