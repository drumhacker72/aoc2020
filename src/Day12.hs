module Day12 (day12) where

import Control.Monad (foldM)
import Control.Monad.Trans.State (evalState, get, modify, put)
import Day (statelessDay)

type Nav = (Action, Int)
data Action = N | S | E | W | L | R | F
    deriving (Read, Show)

readNav (a:n) = (read [a], read n)
readNavs = map readNav . lines

move1 p (F, n) = do
    r <- get
    let a' = case r `mod` 360 of
            0 -> E
            90 -> N
            180 -> W
            270 -> S
    move1 p (a', n)
move1 p@(x, y) (a, n) = case a of
    N -> return (x, y+n)
    S -> return (x, y-n)
    E -> return (x+n, y)
    W -> return (x-n, y)
    L -> modify (+ n) >> return p
    R -> modify (subtract n) >> return p

move2 (sx, sy) (F, n) = do
    (wx, wy) <- get
    return (sx+wx*n, sy+wy*n)
move2 sp (a, n) = do
    (wx, wy) <- get
    case a of
        N -> put (wx, wy+n)
        S -> put (wx, wy-n)
        E -> put (wx+n, wy)
        W -> put (wx-n, wy)
        L -> case n of
            90 -> put (-wy, wx)
            180 -> put (-wx, -wy)
            270 -> put (wy, -wx)
        R -> case n of
            90 -> put (wy, -wx)
            180 -> put (-wx, -wy)
            270 -> put (-wy, wx)
    return sp

manhattan (x, y) = abs x + abs y

day12 = statelessDay readNavs part1 part2
  where
    part1 = show . manhattan . (`evalState` 0) . foldM move1 (0, 0)
    part2 = show . manhattan . (`evalState` (10, 1)) . foldM move2 (0, 0)
