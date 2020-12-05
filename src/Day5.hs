import Numeric (readInt)
import qualified Data.IntSet as IS

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

main = do
    seats <- fmap lines getContents
    let seatIds = map (seatId . readLoc) seats
    print $ maximum seatIds
    print $ findGap $ IS.fromList seatIds
