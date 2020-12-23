module Day23 (day23) where

import Data.Function ((&))
import Data.IntMap.Strict ((!))
import qualified Data.IntMap.Strict as IM
import Day (statelessDay)

type Cup = Int
readCups :: String -> [Cup]
readCups s = map (\x -> read [x]) line
  where
    [line] = lines s

target maxCup i pickup
    | i == 0          = target maxCup maxCup pickup
    | i `elem` pickup = target maxCup (i-1) pickup
    | otherwise       = i

move maxCup current circle =
    let pickup1 = circle ! current
        pickup2 = circle ! pickup1
        pickup3 = circle ! pickup2
        dest = target maxCup (current-1) [pickup1, pickup2, pickup3]
     in circle
        & IM.insert current (circle ! pickup3)
        & IM.insert dest pickup1
        & IM.insert pickup3 (circle ! dest)

run 0 _ _ circle = circle
run i maxCup current circle =
    let circle' = move maxCup current circle
     in run (i-1) maxCup (circle' ! current) circle'

from cup circle =
    let cup' = (circle ! cup)
     in cup' : from cup' circle

day23 = statelessDay readCups part1 part2
  where
    part1 cups =
        let circle = IM.fromList $ zip cups (tail cups ++ [head cups])
         in concatMap show $ take 8 $ from 1 $ run 100 9 (head cups) circle
    part2 cups =
        let cups' = cups ++ [10..1000000]
            circle = IM.fromList $ zip cups' (tail cups' ++ [head cups])
         in show $ product $ take 2 $ from 1 $ run 10000000 1000000 (head cups') circle
