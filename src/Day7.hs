module Day7(Day7) where

import Control.Monad.Trans.Reader (Reader, ask, runReader)
import Data.Bifunctor (first)
import Data.Char (isAlpha, isDigit)
import Data.Map (Map, fromList, keys, (!))
import Text.ParserCombinators.ReadP (ReadP, (+++))
import qualified Text.ParserCombinators.ReadP as P
import Day

type Ruleset = Map String [BagCount]
type BagRule = (String, [BagCount])
type BagCount = (Int, String)

bagContents :: ReadP [BagCount]
bagContents = (P.string "no other bags" >> return []) +++ P.sepBy1 bagCount (P.string ", ")

bagColor :: ReadP String
bagColor = P.many1 $ P.satisfy $ \c -> c == ' ' || isAlpha c

bagCount :: ReadP BagCount
bagCount = do
    num <- read <$> P.munch1 isDigit
    P.char ' '
    color <- bagColor
    P.string " bag"
    P.optional $ P.char 's'
    return (num, color)

bagRule :: ReadP BagRule
bagRule = do
    color <- bagColor
    P.string " bags contain "
    contents <- bagContents
    P.char '.'
    return (color, contents)

readRule :: String -> BagRule
readRule = (\[(r, "")] -> r) . P.readP_to_S bagRule

canContain :: String -> String -> Reader Ruleset Bool
test `canContain` search = do
    testContents <- map snd . (! test) <$> ask
    or <$> mapM (\c -> (c == search ||) <$> c `canContain` search) testContents

countContainers :: String -> Reader Ruleset Int
countContainers search = do
    colors <- keys <$> ask
    length . filter id <$> mapM (`canContain` search) colors

explodeSome :: BagCount -> Reader Ruleset [BagCount]
explodeSome (num, color) = do
    rules <- ask
    return $ map (first (* num)) $ rules ! color

explodeCount :: BagCount -> Reader Ruleset Int
explodeCount bc = do
    contents <- explodeSome bc
    contentsCount <- sum <$> mapM explodeCount contents
    return $ sum (map fst contents) + contentsCount

newtype Day7 = D7 { runD7 :: Ruleset }
instance Day Day7 where
    readDay _ = D7 . fromList . map readRule . lines
    part1 = show . runReader (countContainers "shiny gold") . runD7
    part2 = show . runReader (explodeCount (1, "shiny gold")) . runD7
