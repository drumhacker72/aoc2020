module Day11 (day11) where

import Control.Comonad (Comonad(extend, extract))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Day (statelessDay)
import Day11.Grid (Grid, down, fromVector, left, right, toVector, up)

type SeatLayout = Vector (Vector Char)
readSeatLayout = V.fromList . map V.fromList . lines

adjacents :: [Grid a -> Grid a]
adjacents = horizontals ++ verticals ++ diagonals
  where
    horizontals = [left, right]
    verticals = [up, down]
    diagonals = [ h . v | v <- verticals, h <- horizontals ]

visibleSeat :: (Grid Char -> Grid Char) -> Grid Char -> Maybe (Grid Char)
visibleSeat dir g =
    let g' = dir g
     in case extract g' of
        ' ' -> Nothing
        '.' -> visibleSeat dir g'
        _   -> Just g'

occupied :: Grid Char -> Bool
occupied = ('#' ==) . extract

countTrue :: [Bool] -> Int
countTrue = length . filter id

rule1 :: Grid Char -> Char
rule1 g = case (extract g
               , countTrue $ map (occupied . ($ g)) adjacents
               ) of
    ('L', 0)          -> '#'
    ('#', x) | x >= 4 -> 'L'
    (s,   _)          -> s

rule2 :: Grid Char -> Char
rule2 g = case ( extract g
               , countTrue $ map (maybe False occupied . ($ g) . visibleSeat) adjacents
               ) of
    ('L', 0)          -> '#'
    ('#', x) | x >= 5 -> 'L'
    (s,   _)          -> s

run :: (Grid Char -> Char) -> SeatLayout -> Int
run rule g =
    let g' = toVector $ extend rule $ fromVector ' ' g
     in if g == g'
        then sum $ V.map (length . V.filter id . V.map ('#' ==)) g
        else run rule g'

day11 = statelessDay readSeatLayout part1 part2
  where
    part1 = show . run rule1
    part2 = show . run rule2
