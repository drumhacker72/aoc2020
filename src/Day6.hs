module Day6(Day6) where

import Data.Char (isLower)
import qualified Data.Set as S
import qualified Text.ParserCombinators.ReadP as P
import Day

type Group = [S.Set Char]
questions = S.fromList <$> P.munch1 isLower
group = P.sepBy1 questions (P.char '\n')
groups = P.sepBy1 group (P.string "\n\n")
readGroups s = case P.readP_to_S (groups <* P.skipSpaces <* P.eof) s of [(gs, "")] -> gs

newtype Day6 = D6 { runD6 :: [Group] }
instance Day Day6 where
    readDay _ = D6 . readGroups
    part1 = show . sum . map (S.size . S.unions) . runD6
    part2 = show . sum . map (S.size . foldl1 S.intersection) . runD6
