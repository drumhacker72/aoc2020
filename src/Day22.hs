module Day22 (day22) where

import Data.Char (isDigit)
import Data.Set (Set)
import Data.Sequence (Seq(Empty, (:<|), (:|>)))
import Text.ParserCombinators.ReadP (ReadP, (+++))
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Text.ParserCombinators.ReadP as P
import Day (statelessDay)

type Card = Int

card :: ReadP Card
card = read <$> P.munch1 isDigit
deck = do
    P.string "Player "
    P.char '1' +++ P.char '2'
    P.string ":\n"
    Seq.fromList <$> P.sepBy1 card (P.char '\n')
decks = do
    d1 <- deck
    P.skipSpaces
    d2 <- deck
    return (d1, d2)
readDecks s = case P.readP_to_S (decks <* P.skipSpaces <* P.eof) s of [(ds, "")] -> ds

play d1 Empty = score 1 0 d1
play Empty d2 = score 1 0 d2
play (c1 :<| d1) (c2 :<| d2)
    | c1 > c2 = play (d1 :|> c1 :|> c2) d2
    | otherwise = play d1 (d2 :|> c2 :|> c1)

score _ i Empty = i
score m i (d :|> c) = score (m+1) (i + m*c) d

playR d1 Empty _ = Left $ score 1 0 d1
playR Empty d2 _ = Right $ score 1 0 d2
playR d1 d2 seen
    | (d1, d2) `Set.member` seen = Left $ score 1 0 d1
playR a1@(c1 :<| d1) a2@(c2 :<| d2) seen
    | length d1 >= c1 && length d2 >= c2 =
        case playR (Seq.take c1 d1) (Seq.take c2 d2) Set.empty of
            Left _ -> playR (d1 :|> c1 :|> c2) d2 seen'
            Right _ -> playR d1 (d2 :|> c2 :|> c1) seen'
    | c1 > c2 = playR (d1 :|> c1 :|> c2) d2 seen'
    | otherwise = playR d1 (d2 :|> c2 :|> c1) seen'
  where
    seen' = Set.insert (a1, a2) seen

day22 = statelessDay readDecks part1 part2
  where
    part1 (d1, d2) = show $ play d1 d2
    part2 (d1, d2) = either show show $ playR d1 d2 Set.empty
