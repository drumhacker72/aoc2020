module Day1 (day1) where

import Day (statelessDay)

type Entry = Int
readEntries = map (read :: String -> Int) . lines

find2 entries = head
    [ e1 * e2 | e1 <- entries, e2 <- entries, e1 + e2 == 2020 ]

find3 entries = head
    [ e1 * e2 * e3 | e1 <- entries, e2 <- entries, e3 <- entries, e1 + e2 + e3 == 2020 ]

day1 = statelessDay readEntries part1 part2
  where
    part1 = show . find2
    part2 = show . find3
