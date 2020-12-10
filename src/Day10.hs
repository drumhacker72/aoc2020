module Day10(Day10) where

import Data.List (sort)
import Day

type Adapter = Int

downFrom :: [Adapter] -> Adapter
downFrom adapters = df 0
  where
    df = (map df' [0..] !!)
    df' :: Int -> Int
    df' i
        | i == length adapters - 1 = 1
        | otherwise =
            let (a:as) = drop i adapters
                nb = if length as > 0 && as !! 0 `elem` [a-1, a-2, a-3] then df (i+1) else 0
                nc = if length as > 1 && as !! 1 `elem` [a-2, a-3] then df (i+2) else 0
                nd = if length as > 2 && as !! 2 `elem` [a-3] then df (i+3) else 0
             in nb + nc + nd

diff (a1:a2:as) = (a1-a2) : diff (a2:as)
diff _ = []

newtype Day10 = D10 { runD10 :: [Adapter] }
instance Day Day10 where
    readDay _ = D10 . reverse . (0:) . sort . map (read :: String -> Int) . lines
    part1 (D10 adapters) =
        let adapters' = maximum adapters + 3 : adapters
         in show $ length (filter (== 1) $ diff adapters') * length (filter (== 3) $ diff adapters')
    part2 = show . downFrom . runD10
