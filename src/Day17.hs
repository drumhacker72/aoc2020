module Day17 (day17) where

import qualified Data.Set as S
import Day (statelessDay)
import Day17.BoolGrid (BoolGrid)
import Day17.Vec (Vec, Vec3, Vec4, neighbors)
import qualified Day17.BoolGrid as BG

readCell '.' = False
readCell '#' = True

readSlice = map (map readCell) . lines

candidates grid =
    let ps = BG.elems grid
        ns = map (S.fromList . neighbors) $ S.toList ps
     in S.toList $ S.unions (ps:ns)

evolve grid = BG.fromList $ map (\p -> (p, evolve' p (BG.get p grid))) $ candidates grid
  where
    evolve' p v =
        let n = length $ filter id $ map (`BG.get` grid) $ neighbors p
         in case (v, n) of
             (True, 2) -> True
             (_,    3) -> True
             _         -> False

run :: (Ord v, Vec v) => BoolGrid v -> String
run = show . BG.count . (!! 6) . iterate evolve

day17 = statelessDay readSlice part1 part2
  where
    part1 slice = run (BG.from2dSlice slice :: BoolGrid Vec3)
    part2 slice = run (BG.from2dSlice slice :: BoolGrid Vec4)
