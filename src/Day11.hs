module Day11(Day11) where

import Control.Comonad (Comonad(extend, extract))
import Data.Grid (Grid)
import Data.Vector (Vector)
import qualified Data.Grid as G
import qualified Data.Vector as V
import Day

type SeatLayout = Vector (Vector Char)
readSeatLayout = V.fromList . map V.fromList . lines

adjacents :: [Grid a -> Grid a]
adjacents = horizontals ++ verticals ++ diagonals
  where
    horizontals = [G.left, G.right]
    verticals = [G.up, G.down]
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

rule1 :: Grid Char -> Char
rule1 g = case (extract g, length $ filter id $ map (occupied . ($ g)) adjacents) of
    ('L', 0)          -> '#'
    ('#', x) | x >= 4 -> 'L'
    (s,   _)          -> s

rule2 :: Grid Char -> Char
rule2 g = case ( extract g
               , length $ filter id $ map (maybe False occupied . ($ g) . visibleSeat) adjacents
               ) of
    ('L', 0)          -> '#'
    ('#', x) | x >= 5 -> 'L'
    (s,   _)          -> s

run :: (Grid Char -> Char) -> SeatLayout -> Int
run rule g =
    let g' = G.toVector $ extend rule $ G.fromVector ' ' g
     in if g == g'
        then sum $ V.map (length . V.filter id . V.map ('#' ==)) g
        else run rule g'

newtype Day11 = D11 { runD11 :: SeatLayout }
instance Day Day11 where
    readDay _ = D11 . readSeatLayout
    part1 = show . run rule1 . runD11
    part2 = show . run rule2 . runD11
