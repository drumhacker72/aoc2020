module Day16 where --(Day16) where

import Control.Monad (guard)
import Data.Char (isDigit, isLower)
import Data.Foldable (toList)
import Data.IntSet (IntSet)
import Data.List (isPrefixOf, permutations)
import Data.Sequence (Seq((:|>), (:<|), Empty), (><))
import qualified Data.IntSet as IS
import qualified Data.Sequence as S
import qualified Text.ParserCombinators.ReadP as P
import Day

data Rule = Rule { field :: String, predicate :: Int -> Bool }
type Ticket = [Int]

number = read <$> P.munch1 isDigit
rule = do
    name <- P.munch1 $ \c -> isLower c || c == ' '
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

canBeValid preds v = any ($ v) preds

ticketErrorRate preds ticket =
    sum $ map (\v -> if canBeValid preds v then 0 else v) ticket

validTicket preds = all (canBeValid preds)

validOrders :: IntSet -> Int -> [Seq Int] -> Seq Rule -> [Seq Rule]
validOrders used t tickets rules
    | IS.size used == length rules = [S.empty]
    | otherwise = do
        r <- [0 .. length rules - 1]
        guard $ r `IS.notMember` used
        let rule = rules `S.index` r
        guard $ all (predicate rule . (`S.index` t)) tickets
        rest' <- validOrders (IS.insert r used) (t+1) tickets rules
        return $ rule :<| rest'

data Day16 = D16 [Rule] Ticket [Ticket]
instance Day Day16 where
    readDay _ s =
        let [((rules, your, nearby), "")] = P.readP_to_S input s
         in D16 rules your nearby
    part1 (D16 rules _ nearby) =
        let preds = map predicate rules
         in show $ sum $ map (ticketErrorRate preds) nearby
    part2 (D16 rules your nearby) =
        let preds = map predicate rules
            nearby' = filter (validTicket preds) nearby
            rules' = toList $ head $ validOrders IS.empty 0 (map S.fromList (your:nearby')) $ S.fromList rules
            your' = zip (map field rules') your
            departures = filter (("departure" `isPrefixOf`) . fst) your'
         in show $ product $ map snd departures
