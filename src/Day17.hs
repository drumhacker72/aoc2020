module Day17 (day17) where

import Control.Comonad (Comonad(extend, extract))
import Control.Comonad.Trans.Store (Store, experiment, peek, pos, runStore, seek, seeks, store)
import Data.Set (Set)
import Data.MemoTrie (HasTrie, memo)
import qualified Data.Set as S
import Day (statelessDay)
import Day17.Vec (Vec, Vec3(Vec3), Vec4(Vec4))
import qualified Day17.Vec as V

readCell '.' = False
readCell '#' = True

readSlice = map (map readCell)
readSlices = readSlice . lines

tab :: HasTrie v => Store v a -> Store v a
tab g =
    let (f, s) = runStore g
     in store (memo f) s

iters :: Num a => a
iters = 6

evolve :: (HasTrie v, Vec v) => Store v Bool -> [Store v Bool]
evolve = iterate (tab . extend rule)

window :: Vec v => v -> v -> Store v a -> [a]
window lower upper = experiment $ \v -> map (v `V.add`) $ V.between lower upper

from2dSlice :: (HasTrie v, Ord v, Vec v) => [[Bool]] -> Store v Bool
from2dSlice rows =
    let tagRow y = zipWith (\x v -> (V.from2d (x, y), v)) [0..]
     in mkBoolGrid $ S.fromList $ map fst $ filter snd $ concat $ zipWith tagRow [0..] rows

mkBoolGrid :: (HasTrie v, Ord v, Vec v) => Set v -> Store v Bool
mkBoolGrid s = store (memo (`S.member` s)) V.zero

countNeighbors :: (HasTrie v, Vec v) => Store v Bool -> Int
countNeighbors g = length $ filter id $ map (`peek` g) $ V.neighbors $ pos g

rule :: (HasTrie v, Vec v) => Store v Bool -> Bool
rule g = case (extract g, countNeighbors g) of
    (True, 2) -> True
    (_,    3) -> True
    _         -> False

day17 = statelessDay readSlices part1 part2
  where
    part1 slices =
        let w = fromIntegral $ length (head slices)
            g = evolve (from2dSlice slices) !! iters
            lower = Vec3 (-iters) (-iters) (-iters)
            upper = Vec3 (w-1+iters) (w-1+iters) iters
         in show $ length $ filter id $ window lower upper g
    part2 slices =
        let w = fromIntegral $ length (head slices)
            g = evolve (from2dSlice slices) !! iters
            lower = Vec4 (-iters) (-iters) (-iters) (-iters)
            upper = Vec4 (w-1+iters) (w-1+iters) iters iters
         in show $ length $ filter id $ window lower upper g
