module Day6(Day6) where

import Data.List.Split (splitWhen)
import qualified Data.Set as S
import Day

type Group = [S.Set Char]

newtype Day6 = D6 { runD6 :: [Group] }
instance Day Day6 where
    readDay _ = D6 . map (map S.fromList) . splitWhen null . lines
    part1 = show . sum . map (S.size . S.unions) . runD6
    part2 = show . sum . map (S.size . foldl1 S.intersection) . runD6
