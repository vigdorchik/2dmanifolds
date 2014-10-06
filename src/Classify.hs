{-# LANGUAGE TupleSections #-}
module Classify (normalize) where

import Classify.Cursor
import Data.List
import Data.Array
import Data.Maybe
import Control.Monad
import Control.Arrow (left)
import Control.Applicative ((<|>))
import Text.ParserCombinators.Parsec (many, char)
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Char (letter)
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

data Canonical = Zero | Crosscaps Int | Handles Int deriving Show

mkCanonical crosscaps handles
  | crosscaps == 0 && handles == 0 = Zero
  | crosscaps > 0 = Crosscaps $ crosscaps+2*handles
  | otherwise = Handles handles

edgeP = do
  c <- letter
  option (E c) $ fmap (const (E_1 c)) (char '\'')
holeP = fmap Hole $ many1 edgeP
sphereP = fmap Sphere $ many1 (between (char '(') (char ')') holeP)
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

findTwin :: Edge -> [Sphere] -> Maybe ((Cursor Edge, Cursor Hole), Cursor Sphere)
findTwin e =
  msum . (map (\cs@(_,s,_) -> fmap (, cs) $ findTwin' e (holes s))) . cursors

findTwin' :: Edge -> [Hole] -> Maybe (Cursor Edge, Cursor Hole)
findTwin' e =
  msum . (map (\ch@(_,h,_) -> fmap (, ch) $ findTwin'' e (edges h))) . cursors

findTwin'' :: Edge -> [Edge] -> Maybe (Cursor Edge)
findTwin'' e = find ((==) (edgeLabel e) . edgeLabel . pointed) . cursors

inv (E u) = E_1 u
inv (E_1 u) = E u
invert = reverse . (map inv)

sameDir :: Edge -> Edge -> Bool
sameDir (E _) (E _) = True
sameDir (E_1 _) (E_1 _) = True
sameDir _ _ = False

align e e' es (pre,_,post)
  | sameDir e e' = (invert pre)++(invert post)++es
  | otherwise = post++pre++es

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
                  Just ((ce, ch), cs) ->
                    let ss = excluded cs
                        hs = (excluded ch)++newHoles
                        aligned = align e (pointed ce) tl ce in
                    doEdges aligned doneEdges hs ss
                doEdges [] doneEdges newHoles spheres =
                  (Hole $ reverse doneEdges, newHoles, spheres)
        in doHoles holes' [] spheres

{- check that the start symbol participates in the pattern.
   If so, return the unmatched part -}
matchPattern :: Int -> ([Edge] -> Bool) -> [Edge] -> Maybe [Edge]
matchPattern len equiv es =
  let les = length es
      arr = listArray (0,les-1) es in
  if les < len then Nothing else
    let match = find (\j -> equiv [arr ! (i `mod` les)| i <- [j-len+1..j]]) [0..len-1] in
    fmap (\j -> take (les-len) (drop (j+1) es)) match

handle = matchPattern 4 handleEquiv where
  handleEquiv [a, b, a', b']| dual a a' && dual b b' = True  where
    dual (E a) (E_1 a')| a == a' = True
    dual (E_1 a) (E a')| a == a' = True
    dual _ _ = False
  handleEquiv _ = False
    
crosscap = matchPattern 2 crosscapEquiv where
  crosscapEquiv [E a, E a']| a == a' = True
  crosscapEquiv [E_1 a, E_1 a']| a == a' = True
  crosscapEquiv _ = False

normalizeSphere :: Sphere -> String
normalizeSphere =
  let doHoles :: Int -> Int -> [Hole] -> Canonical
      doHoles crosscaps handles [] = mkCanonical crosscaps handles
      doHoles crosscaps handles (h:hs) =
        let doEdges crosscaps handles [] = doHoles crosscaps handles hs
            doEdges crosscaps handles (all@(e:es)) =
              let separated = fmap (doEdges (crosscaps+1) handles) (crosscap all) <|>
                              fmap (doEdges crosscaps (handles+1)) (handle all) in
              maybe doZip id separated where
                doZip = case findTwin'' e es of
                  Just (pre, e', post)  ->
                    if sameDir e e' then
                      doEdges (crosscaps+1) handles ((invert pre)++post)
                    else
                      doHoles crosscaps handles ((Hole pre):(Hole post):hs)
                  Nothing ->
                    let Just ((pre, e', post), ch') = findTwin' e hs in
                    if sameDir e e' then {- crosshandle -}
                      doHoles (crosscaps+2) handles ((Hole $ post++pre++(invert es)):(excluded ch'))
                    else {- handle -}
                      doHoles crosscaps (handles+1) ((Hole $ post++pre++es):(excluded ch'))  in
        doEdges crosscaps handles (edges h)
  in show . doHoles 0 0 . holes
