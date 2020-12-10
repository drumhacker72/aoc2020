module Day1(Day1) where

import Day

type Entry = Int
readEntries = map (read :: String -> Int) . lines

find2 entries = head
    [ e1 * e2 | e1 <- entries, e2 <- entries, e1 + e2 == 2020 ]

find3 entries = head
    [ e1 * e2 * e3 | e1 <- entries, e2 <- entries, e3 <- entries, e1 + e2 + e3 == 2020 ]

newtype Day1 = D1 { runD1 :: [Entry] }
instance Day Day1 where
    readDay _ = D1 . readEntries
    part1 = show . find2 . runD1
    part2 = show . find3 . runD1
