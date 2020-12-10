module Day2(Day2) where

import Data.Char (isDigit)
import qualified Text.ParserCombinators.ReadP as P
import Day

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

newtype Day2 = D2 { runD2 :: [Entry] }
instance Day Day2 where
    readDay _ = D2 . readEntries
    part1 = show . length . filter isValid1 . runD2
    part2 = show . length . filter isValid2 . runD2
