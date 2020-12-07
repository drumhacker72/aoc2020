import Control.Monad (unless, when)
import Control.Monad.State (State, execState, get, modify, put)
import Data.Bifunctor (first)
import Data.Char (isAlpha, isDigit)
import Data.List (find)
import Data.Map (Map, (!))
import Data.Set (Set, insert, member)
import Text.ParserCombinators.ReadP (ReadP, readP_to_S, (+++))

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Text.ParserCombinators.ReadP as P

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
readRule = (\[(r, "")] -> r) . readP_to_S bagRule

tryMatch :: BagRule -> State (Set String) Bool
tryMatch (color, contents) = do
    search <- get
    if color `member` search
    then return False
    else case find (\(_, c) -> c `member` search) contents of
        Just _  -> modify (insert color) >> return True
        Nothing -> return False

expandMatches :: [BagRule] -> State (Set String) ()
expandMatches rules = do
    newMatches <- mapM tryMatch rules
    when (or newMatches) $ expandMatches rules

explodeOne :: Map String [BagCount] -> BagCount -> [BagCount]
explodeOne rules (num, color) = map (first (* num)) $ rules ! color

explode :: Map String [BagCount] -> State (Int, [BagCount]) ()
explode rules = do
    (count, remaining) <- get
    let count' = count + sum (map fst remaining)
    let remaining' = concatMap (explodeOne rules) remaining
    put (count', remaining')
    unless (null remaining') $ explode rules

main = do
    rules <- map readRule . lines <$> getContents
    print $ length (execState (expandMatches rules) $ S.fromList ["shiny gold"]) - 1
    print $ fst (execState (explode $ M.fromList rules) (0, [(1, "shiny gold")])) - 1
