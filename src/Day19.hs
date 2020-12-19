module Day19(Day19) where

import Data.Char (isDigit)
import Data.IntMap.Strict (IntMap, (!))
import Text.ParserCombinators.ReadP ((+++))
import qualified Data.IntMap.Strict as IM
import qualified Text.ParserCombinators.ReadP as P
import Day

data Rule = CharRule Char | MatchRule [[Int]]
    deriving Show

number = read <$> P.munch1 isDigit
charRule = P.char '"' >> fmap CharRule P.get <* P.char '"'
subruleList = P.sepBy1 number (P.char ' ')
matchRule = MatchRule <$> P.sepBy1 subruleList (P.string " | ")
numberedRule = do
    n <- number
    P.string ": "
    r <- charRule +++ matchRule
    return (n, r)
input = do
    numberedRules <- P.sepBy1 numberedRule (P.char '\n')
    P.string "\n\n"
    messages <- lines <$> P.munch (const True)
    return (IM.fromList numberedRules, messages)
readNumberedRule s = case P.readP_to_S (numberedRule <* P.eof) s of [(x, "")] -> x
readInput s = case P.readP_to_S input s of [(x, "")] -> x

match ruleMap (x:xs) (CharRule c:rs) =
    if x == c then match ruleMap xs rs else []
match ruleMap xs (MatchRule ms:rs) = do
    m <- ms
    match ruleMap xs (map (ruleMap !) m ++ rs)
match _ xs [] = [xs]
match _ "" (_:_) = []

fullMatch ruleMap xs rs = "" `elem` match ruleMap xs rs

data Day19 = D19 (IntMap Rule) [String]
instance Day Day19 where
    readDay _ = uncurry D19 . readInput
    part1 (D19 ruleMap msgs) =
        show $ length $ filter id $ map (\msg -> fullMatch ruleMap msg [ruleMap ! 0]) msgs
    part2 (D19 ruleMap msgs) =
        let newRules = map readNumberedRule
                [ "8: 42 | 42 8"
                , "11: 42 31 | 42 11 31"
                ]
            ruleMap' = IM.union (IM.fromList newRules) ruleMap
         in show $ length $ filter id $ map (\msg -> fullMatch ruleMap' msg [ruleMap ! 0]) msgs
