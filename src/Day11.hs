module Day11(Day11) where

import Control.Comonad (Comonad(extend, extract))
import Data.Grid as G
import Debug.Trace
import Day

pack :: [[Char]] -> (Int, Int, G.Grid Char)
pack rows = (length rows, length (head rows), G.fromList ' ' rows)

unpack :: Int -> Int -> G.Grid Char -> [[Char]]
unpack h w = G.toList 0 (h-1) 0 (w-1)

adjacents :: [G.Grid a -> G.Grid a]
adjacents = horizontals ++ verticals ++ diagonals
  where
    horizontals = [G.left, G.right]
    verticals = [G.up, G.down]
    diagonals = [ h . v | v <- verticals, h <- horizontals ]

visibleSeat :: (G.Grid Char -> G.Grid Char) -> G.Grid Char -> Maybe (G.Grid Char)
visibleSeat dir g =
    let g' = dir g
     in case extract g' of
        ' ' -> Nothing
        '.' -> visibleSeat dir g'
        _   -> Just g'

occupied :: G.Grid Char -> Bool
occupied = ('#' ==) . extract

rule1 :: G.Grid Char -> Char
rule1 g = case (extract g, length $ filter id $ map (occupied . ($ g)) adjacents) of
    ('L', 0)          -> '#'
    ('#', x) | x >= 4 -> 'L'
    (s,   _)          -> s

rule2 :: G.Grid Char -> Char
rule2 g = case ( extract g
               , length $ filter id $ map (maybe False occupied . ($ g) . visibleSeat) adjacents
               ) of
    ('L', 0)          -> '#'
    ('#', x) | x >= 5 -> 'L'
    (s,   _)          -> s

run rule h w g u =
    let g' = extend rule g
        u' = unpack h w g'
     in if u == u'
        then sum $ map (length . filter id . map ('#' ==)) u
        else run rule h w g' u'

data Day11 = D11 Int Int (G.Grid Char)
instance Day Day11 where
    readDay _ s =
        let (h, w, g) = pack $ lines s
         in D11 h w g
    part1 (D11 h w g) = show $ run rule1 h w g (unpack h w g)
    part2 (D11 h w g) = show $ run rule2 h w g (unpack h w g)
