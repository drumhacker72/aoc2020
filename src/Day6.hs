module Day6 (day6) where

import Data.Char (isLower)
import qualified Data.Set as S
import qualified Text.ParserCombinators.ReadP as P
import Day (statelessDay)

type Group = [S.Set Char]
questions = S.fromList <$> P.munch1 isLower
group = P.sepBy1 questions (P.char '\n')
groups = P.sepBy1 group (P.string "\n\n")
readGroups s = case P.readP_to_S (groups <* P.skipSpaces <* P.eof) s of [(gs, "")] -> gs

day6 = statelessDay readGroups part1 part2
  where
    part1 = show . sum . map (S.size . S.unions)
    part2 = show . sum . map (S.size . foldl1 S.intersection)
