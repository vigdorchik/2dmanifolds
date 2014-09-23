module Classify.Cursor (Cursor, moveL, moveR, cursors, pointed, excluded) where

type Cursor a = ([a], a, [a])

moveL :: Cursor a -> Cursor a
moveL (_, _, []) = error "already at left-most position"
moveL (pre, x, (y:post)) = (x:pre, y, post)

moveR :: Cursor a -> Cursor a
moveR ([], _, _) = error "already at right-most position"
moveR ((y:pre), x, post) = (pre, y, x:post)

cursors :: [a] -> [Cursor a]
cursors [] = []
cursors (hd:tl) = take (length tl + 1) $ iterate moveL ([], hd, tl)

pointed :: Cursor a -> a
pointed (_,x,_) = x

excluded :: Cursor a -> [a]
excluded (l,_,r) = rappend l r where
  rappend [] r = r
  rappend (hd:tl) r = rappend tl (hd:r)
