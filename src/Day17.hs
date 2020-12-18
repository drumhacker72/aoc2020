{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Day17(Day17) where

import Control.Comonad (extend, extract)
import Data.Grid (fromSlice, neighbors, toList3, toList4)
import Day

readCell '.' = False
readCell '#' = True

readSlice = map (map readCell)

data NeighborCount = Two | Three | Other

activeNeighbors g = count $ filter id $ map extract $ neighbors g
  where
    count [_, _]    = Two
    count [_, _, _] = Three
    count _         = Other

rule g = case (extract g, activeNeighbors g) of
    (True,  Two)   -> True
    (True,  Three) -> True
    (False, Three) -> True
    _              -> False

evolve = extend rule

count3 = sum . map (sum . map (length . filter id))

count4 = sum . map (sum . map (sum . map (length . filter id)))

run3 i slice = toList3 (i, i, i) (i+w-1, i+h-1, i) $ states !! i
  where
    h = length slice
    w = length $ head slice
    states = iterate evolve $ fromSlice False slice

run4 i slice = toList4 (i, i, i, i) (i+w-1, i+h-1, i, i) $ states !! i
  where
    h = length slice
    w = length $ head slice
    states = iterate evolve $ fromSlice False slice

newtype Day17 = D17 { runD17 :: [[Bool]] }
instance Day Day17 where
    readDay _ = D17 . readSlice . lines
    part1 = show . count3 . run3 6 . runD17
    part2 = show . count4 . run4 6 . runD17
