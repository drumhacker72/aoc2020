module Day16 (day16) where

import Data.Char (isDigit)
import Data.Foldable (toList)
import Data.List (isPrefixOf, transpose)
import Data.Sequence (Seq((:<|), Empty), (><))
import qualified Data.Sequence as S
import qualified Text.ParserCombinators.ReadP as P
import Day (statelessDay)

data Rule = Rule { field :: String, predicate :: Int -> Bool }
type Ticket = [Int]

number = read <$> P.munch1 isDigit
rule = do
    name <- P.munch1 (/= ':')
    P.string ": "
    n1 <- number
    P.char '-'
    n2 <- number
    P.string " or "
    n3 <- number
    P.char '-'
    n4 <- number
    return $ Rule name $ \i -> (n1 <= i && i <= n2) || (n3 <= i && i <= n4)
ticket = P.sepBy1 number (P.char ',')
input = do
    rules <- P.sepBy1 rule (P.char '\n')
    P.string "\n\nyour ticket:\n"
    your <- ticket
    P.string "\n\nnearby tickets:\n"
    nearby <- P.sepBy1 ticket (P.char '\n')
    P.skipSpaces
    P.eof
    return (rules, your, nearby)

readInput s = case P.readP_to_S input s of [(i, "")] -> i

canBeValid rules v = any (`predicate` v) rules

errorRate rules ticket =
    sum $ map (\v -> if canBeValid rules v then 0 else v) ticket

validOrders [] Empty = [S.empty]
validOrders (ticketSlice:remainingSlices) rules =
    [ rule :<| validRemainingOrder
    | i <- [0 .. length rules - 1]
    , let (left, rule :<| right) = S.splitAt i rules
          remainingRules = left >< right
    , all (predicate rule) ticketSlice
    , validRemainingOrder <- validOrders remainingSlices remainingRules
    ]

findOrder tickets rules =
    toList $ head $ validOrders (transpose tickets) (S.fromList rules)

day16 = statelessDay readInput part1 part2
  where
    part1 (rules, _, nearby) = show $ sum $ map (errorRate rules) nearby
    part2 (rules, your, nearby) =
        let validNearby = filter (all (canBeValid rules)) nearby
            fields = map field $ findOrder (your:validNearby) rules
            departureProps = filter (("departure" `isPrefixOf`) . fst) $ zip fields your
         in show $ product $ map snd departureProps
