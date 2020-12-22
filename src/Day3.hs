module Day3 (day3) where

import Day (statelessDay)

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

day3 = statelessDay readGrid part1 part2
  where
    part1 = show . countTreesInLine (3, 1)
    part2 =
        let slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
         in show . product . (\grid -> map (`countTreesInLine` grid) slopes)
