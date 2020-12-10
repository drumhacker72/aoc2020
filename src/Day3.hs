module Day3(Day3) where

import Day

type Grid = [String]
readGrid = map cycle . lines

grid `at` (x, y) = case drop y grid of
    []    -> Nothing
    row:_ -> Just $ row !! x

countTreesInLine (dx, dy) grid = countFrom (0, 0)
  where
    countFrom (x, y) =
        let rest = countFrom (x+dx, y+dy)
         in case grid `at` (x, y) of
                Nothing  -> 0
                Just '.' -> rest
                Just '#' -> 1 + rest

newtype Day3 = D3 { runD3:: Grid }
instance Day Day3 where
    readDay _ = D3 . readGrid
    part1 = show . countTreesInLine (3, 1) . runD3
    part2 =
        let slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
         in show . product . (\grid -> map (`countTreesInLine` grid) slopes) . runD3
