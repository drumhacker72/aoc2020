module Day10 (day10) where

import Data.List (zipWith4)
import qualified Data.Set as S
import Day (statelessDay)

type Adapter = Int
readAdapters = S.fromList . map read . lines

diff (a1:a2:as) = (a2-a1) : diff (a2:as)
diff _ = []

paths :: S.Set Adapter -> Int
paths adapters = paths' !! (S.findMax adapters + 2)
  where
    paths' = 0 : 0 : 1 : zipWith4 next [1..] paths' (drop 1 paths') (drop 2 paths')
    next i p3 p2 p1
        | i `S.member` adapters = p3 + p2 + p1
        | otherwise = 0

day10 = statelessDay readAdapters part1 part2
  where
    part1 as =
        let as' = 0 : S.toAscList as ++ [S.findMax as + 3]
         in show $ length (filter (== 1) $ diff as') * length (filter (== 3) $ diff as')
    part2 = show . paths
