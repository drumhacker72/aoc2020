module Day18 (day18) where

import Data.Char (isDigit)
import Text.ParserCombinators.ReadP ((+++))
import qualified Text.ParserCombinators.ReadP as P
import Day (statelessDay)

data Expr
    = Literal Int
    | Add Expr Expr
    | Mult Expr Expr

number = Literal . read <$> P.munch1 isDigit
parenthetical expr = P.char '(' >> P.skipSpaces >> expr <* P.char ')'
term expr = (number +++ parenthetical expr) <* P.skipSpaces
add = P.char '+' >> P.skipSpaces >> return Add
mult = P.char '*' >> P.skipSpaces >> return Mult

exprNoPrec = P.chainl1 (term exprNoPrec) (add +++ mult)

addExpr = P.chainl1 (term exprAddFirst) add
exprAddFirst = P.chainl1 addExpr mult

readExpr expr s = case P.readP_to_S (expr <* P.eof) s of [(e, "")] -> e

run :: Expr -> Int
run (Literal n) = n
run (Add a b) = run a + run b
run (Mult a b) = run a * run b

day18 = statelessDay lines part1 part2
  where
    part1 = show . sum . map (run . readExpr exprNoPrec)
    part2 = show . sum . map (run . readExpr exprAddFirst)
