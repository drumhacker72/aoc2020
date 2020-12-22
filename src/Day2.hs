module Day2 (day2) where

import Data.Char (isDigit)
import qualified Text.ParserCombinators.ReadP as P
import Day (statelessDay)

data Entry = Entry Int Int Char String

number = read <$> P.munch1 isDigit
entry = do
    low <- number
    P.char '-'
    high <- number
    P.char ' '
    c <- P.get
    P.string ": "
    return $ Entry low high c
readEntry = uncurry ($) . head . P.readP_to_S entry
readEntries = map readEntry . lines

isValid1 (Entry min max letter password) = min <= count && count <= max
  where
    count = length $ filter (== letter) password

isValid2 (Entry pos1 pos2 letter password) = at1 /= at2
  where
    at1 = password !! (pos1-1) == letter
    at2 = password !! (pos2-1) == letter

day2 = statelessDay readEntries part1 part2
  where
    part1 = show . length . filter isValid1
    part2 = show . length . filter isValid2
