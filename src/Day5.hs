module Day5 (day5) where

import Numeric (readInt)
import qualified Data.IntSet as IS
import Day (statelessDay)

type SeatId = Int

readRow = readInt 2 (`elem` "FB") rowDigit
  where
    rowDigit 'F' = 0
    rowDigit 'B' = 1
readCol = readInt 2 (`elem` "LR") colDigit
  where
    colDigit 'L' = 0
    colDigit 'R' = 1
readLoc code = head
    [ (row, col) | (row, rest) <- readRow code, (col, "") <- readCol rest ]

seatId (r, c) = r * 8 + c
readSeatIds = map (seatId . readLoc) . lines

findGap ids = head
    [ i | i <- [1..], IS.member (i-1) ids, IS.notMember i ids, IS.member (i+1) ids ]

day5 = statelessDay readSeatIds part1 part2
  where
    part1 = show . maximum
    part2 = show . findGap . IS.fromList
