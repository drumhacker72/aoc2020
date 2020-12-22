{-# LANGUAGE FlexibleContexts #-}

module Day15 (day15) where

import Control.Monad (foldM, forM_)
import Control.Monad.ST (runST)
import Data.Char (isDigit)
import Data.Vector.Generic.Mutable (exchange, write)
import Text.ParserCombinators.ReadP ()
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Text.ParserCombinators.ReadP as P
import Day (statelessDay)

number = read <$> P.munch1 isDigit
numberList = P.sepBy1 number (P.char ',')
readNumberList s = case P.readP_to_S (numberList <* P.skipSpaces <* P.eof) s of
    [(ns, "")] -> ns

next is n i = do
    prev <- exchange is n i
    return $ if prev == 0 then 0 else i - prev

run end ns = do
    is <- VUM.replicate end 0
    let (n, i):inits = reverse $ zip ns [1..]
    forM_ inits $ uncurry $ write is
    foldM (next is) n [i..end-1]

day15 = statelessDay readNumberList part1 part2
  where
    part1 ns = show $ runST $ run 2020 ns
    part2 ns = show $ runST $ run 30000000 ns
