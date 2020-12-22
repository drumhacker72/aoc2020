{-# LANGUAGE TypeApplications #-}

module Day17 (day17) where

import Data.Set (Set)
import qualified Data.Set as S
import Day (statelessDay)
import Day17.BoolGrid (BoolGrid)
import Day17.Vector (Vector, neighbors)
import qualified Day17.BoolGrid as BG

readCell '.' = False
readCell '#' = True

readSlice = map (map readCell)
readSlices = readSlice . lines

candidates grid =
    let ps = BG.elems grid
        ns = map (S.fromList . neighbors) $ S.toList ps
     in S.toList $ S.unions (ps:ns)

evolve grid = BG.fromList $ map (\p -> (p, evolve' p (BG.get p grid))) $ candidates grid
  where
    evolve' p v =
        let n = length $ filter id $ map (`BG.get` grid) $ neighbors p
         in case (v, n) of
             (True,  2) -> True
             (True,  3) -> True
             (False, 3) -> True
             _          -> False

day17 = statelessDay readSlices part1 part2
  where
    part1 = show . BG.count . (!! 6) . iterate evolve . BG.from2dSlice @(Int, Int, Int)
    part2 = show . BG.count . (!! 6) . iterate evolve . BG.from2dSlice @(Int, Int, Int, Int)
