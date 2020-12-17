{-# LANGUAGE TypeApplications #-}

module Day17(Day17) where

import Control.Vector (Vector)
import Data.BoolGrid (BoolGrid)
import Data.Set (Set)
import qualified Data.BoolGrid as BG
import qualified Data.Set as S
import Day

readCell '.' = False
readCell '#' = True

readSlice = map (map readCell)

candidates grid =
    let ps = BG.elems grid
        ns = map (S.fromList . BG.neighbors) $ S.toList ps
     in S.toList $ S.unions (ps:ns)

evolve grid = BG.fromList $ map (\p -> (p, evolve' p (BG.get p grid))) $ candidates grid
  where
    evolve' p v =
        let n = length $ filter id $ map (`BG.get` grid) $ BG.neighbors p
         in case (v, n) of
             (True,  2) -> True
             (True,  3) -> True
             (False, 3) -> True
             _          -> False

newtype Day17 = D17 { runD17 :: [[Bool]] }
instance Day Day17 where
    readDay _ = D17 . readSlice . lines
    part1 = show . BG.count . (!! 6) . iterate evolve . BG.from2dSlice @(Int, Int, Int) . runD17
    part2 = show . BG.count . (!! 6) . iterate evolve . BG.from2dSlice @(Int, Int, Int, Int) . runD17
