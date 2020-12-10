module Day5(Day5) where

import Numeric (readInt)
import qualified Data.IntSet as IS
import Day

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

findGap ids = head
    [ i | i <- [1..], IS.member (i-1) ids, IS.notMember i ids, IS.member (i+1) ids ]

newtype Day5 = D5 { runD5 :: [SeatId] }
instance Day Day5 where
    readDay _ = D5 . map (seatId . readLoc) . lines
    part1 = show . maximum . runD5
    part2 = show . findGap . IS.fromList . runD5
