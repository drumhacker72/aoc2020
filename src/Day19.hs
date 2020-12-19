module Day19(Day19) where

import Control.Applicative (empty)
import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (Reader, ReaderT, ask, mapReaderT, runReader)
import Data.Char (isDigit)
import Data.Functor.Identity (Identity(Identity))
import Data.IntMap.Strict (IntMap, (!))
import Text.ParserCombinators.ReadP ((+++))
import qualified Data.IntMap.Strict as IM
import qualified Text.ParserCombinators.ReadP as P
import Day

data Rule = CharRule Char | AltRule [IdxList]
type IdxList = [Int]
type RuleMap = IntMap Rule

number = read <$> P.munch1 isDigit
charRule = P.char '"' >> fmap CharRule P.get <* P.char '"'
idxList = P.sepBy1 number (P.char ' ')
altRule = AltRule <$> P.sepBy1 idxList (P.string " | ")
numberedRule = do
    n <- number
    P.string ": "
    r <- charRule +++ altRule
    return (n, r)
input = do
    ruleMap <- IM.fromList <$> P.sepBy1 numberedRule (P.char '\n')
    P.string "\n\n"
    messages <- lines <$> P.munch (const True)
    return (ruleMap, messages)

readExact parser s = case P.readP_to_S parser s of [(x, "")] -> x
readNumberedRule = readExact $ numberedRule <* P.eof
readInput = readExact input

parse :: [Rule] -> String -> ReaderT RuleMap [] String
parse [] xs = return xs
parse _  "" = empty
parse (CharRule c:rs) (x:xs) =
    if x == c then parse rs xs else empty
parse (AltRule alts:rs) xs = do
    ruleMap <- ask
    alt <- lift alts
    let altRules = map (ruleMap !) alt
    parse (altRules ++ rs) xs

matchesRule0 :: String -> Reader (IntMap Rule) Bool
matchesRule0 s = mapReaderT (Identity . not . null) $ do
    ruleMap <- ask
    remainder <- parse [ruleMap ! 0] s
    guard $ remainder == ""

data Day19 = D19 (IntMap Rule) [String]
instance Day Day19 where
    readDay _ = uncurry D19 . readInput
    part1 (D19 ruleMap msgs) =
        show $ length $ filter id $ map ((`runReader` ruleMap) . matchesRule0) msgs
    part2 (D19 ruleMap msgs) =
        let newRules = map readNumberedRule
                [ "8: 42 | 42 8"
                , "11: 42 31 | 42 11 31"
                ]
            ruleMap' = IM.union (IM.fromList newRules) ruleMap
         in show $ length $ filter id $ map ((`runReader` ruleMap') . matchesRule0) msgs
