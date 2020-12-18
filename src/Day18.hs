module Day18(Day18) where

import Data.Char (isDigit)
import Text.ParserCombinators.ReadP ((+++))
import qualified Text.ParserCombinators.ReadP as P
import Day

data Expr
    = Literal Int
    | Add Expr Expr
    | Mult Expr Expr

data Token = Number Int | LParen | RParen | Plus | Star

number = Number . read <$> P.munch1 isDigit
lparen = P.char '(' >> return LParen
rparen = P.char ')' >> return RParen
plus = P.char '+' >> return Plus
star = P.char '*' >> return Star
token = number +++ lparen +++ rparen +++ plus +++ star
tokens = do
    ts <- P.many1 (P.skipSpaces >> token)
    P.eof
    return ts
readTokens :: String -> [Token]
readTokens = (\[(tokens, "")] -> tokens) . P.readP_to_S tokens

parse1 :: Maybe Expr -> [Token] -> (Expr, [Token])
parse1 (Just expr) [] = (expr, [])
parse1 Nothing (LParen:ts) = untilRParen Nothing ts
  where
    untilRParen (Just expr) (RParen:ts') = (expr, ts')
    untilRParen expr ts' =
        let (expr', ts'') = parse1 expr ts'
         in untilRParen (Just expr') ts''
parse1 Nothing (Number n:ts) = (Literal n, ts)
parse1 (Just expr) (Plus:ts) =
    let (expr2, ts') = parse1 Nothing ts
     in (Add expr expr2, ts')
parse1 (Just expr) (Star:ts) =
    let (expr2, ts') = parse1 Nothing ts
     in (Mult expr expr2, ts')

parse2 :: Maybe Expr -> [Token] -> (Expr, [Token])
parse2 (Just expr) [] = (expr, [])
parse2 Nothing (LParen:ts) = untilRParen Nothing ts
  where
    untilRParen (Just expr) (RParen:Plus:ts') = parse2 (Just expr) (Plus:ts')
    untilRParen (Just expr) (RParen:ts') = (expr, ts')
    untilRParen expr ts' =
        let (expr', ts'') = parse2 expr ts'
         in untilRParen (Just expr') ts''
parse2 Nothing (Number n:Plus:ts) = parse2 (Just $ Literal n) (Plus:ts)
parse2 Nothing (Number n:ts) = (Literal n, ts)
parse2 (Just expr) (Plus:ts) =
    let (expr2, ts') = parse2 Nothing ts
     in (Add expr expr2, ts')
parse2 (Just expr) (Star:ts) =
    let (expr2, ts') = parse2 Nothing ts
     in (Mult expr expr2, ts')

parseAll parse = loop Nothing
  where
    loop expr ts = case parse expr ts of
        (expr', []) -> expr'
        (expr', ts') -> loop (Just expr') ts'

run :: Expr -> Int
run (Literal n) = n
run (Add a b) = run a + run b
run (Mult a b) = run a * run b

newtype Day18 = D18 { runD18 :: [[Token]] }
instance Day Day18 where
    readDay _ = D18 . map readTokens . lines
    part1 = show . sum . map (run . parseAll parse1) . runD18
    part2 = show . sum . map (run . parseAll parse2) . runD18
