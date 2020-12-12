module Day12(Day12) where

import Day

data Nav = Nav Action Int
data Action = N | S | E | W | L | R | F
    deriving (Read, Show)

readNav (a:n) = Nav (read [a]) (read n)

data Loc1 = Loc1 Int Int Int

move1 (Loc1 x y r) (Nav a n) = case a of
    N -> Loc1 x (y+n) r
    S -> Loc1 x (y-n) r
    E -> Loc1 (x+n) y r
    W -> Loc1 (x-n) y r
    L -> Loc1 x y (r-n)
    R -> Loc1 x y (r+n)
    F -> case r `mod` 360 of
        0 -> Loc1 (x+n) y r
        90 -> Loc1 x (y-n) r
        180 -> Loc1 (x-n) y r
        270 -> Loc1 x (y+n) r

data Loc2 = Loc2 Int Int Int Int

move2 (Loc2 sx sy wx wy) (Nav a n) = case a of
    N -> Loc2 sx sy wx (wy+n)
    S -> Loc2 sx sy wx (wy-n)
    E -> Loc2 sx sy (wx+n) wy
    W -> Loc2 sx sy (wx-n) wy
    L -> case n of
        90 -> Loc2 sx sy (-wy) wx
        180 -> Loc2 sx sy (-wx) (-wy)
        270 -> Loc2 sx sy wy (-wx)
    R -> case n of
        90 -> Loc2 sx sy wy (-wx)
        180 -> Loc2 sx sy (-wx) (-wy)
        270 -> Loc2 sx sy (-wy) wx
    F -> Loc2 (sx+wx*n) (sy+wy*n) wx wy

manhattan1 (Loc1 x y _) = abs x + abs y
manhattan2 (Loc2 sx sy _ _) = abs sx + abs sy

newtype Day12 = D12 { runD12 :: [Nav] }
instance Day Day12 where
    readDay _ = D12 . map readNav . lines
    part1 = show . manhattan1 . foldl move1 (Loc1 0 0 0) . runD12
    part2 = show . manhattan2 . foldl move2 (Loc2 0 0 10 1) . runD12
