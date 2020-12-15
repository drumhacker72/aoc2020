{-# LANGUAGE FlexibleContexts #-}

module Day15(Day15) where

import Control.Monad.ST (runST)
import Data.List.Split (splitOn)
import Data.Vector.Generic.Mutable (exchange, write)
import qualified Data.Vector.Unboxed.Mutable as VUM
import Day

next is i n = do
    prev <- exchange is n i
    return $ if prev == 0 then 0 else i - prev

run end ns = do
    is <- VUM.replicate end 0
    (i, n) <- fill is 1 ns
    loop is i n
  where
    fill _  i [n]    = return (i, n)
    fill is i (n:ns) = write is n i >> fill is (i+1) ns

    loop is i n
        | i == end  = return n
        | otherwise = do
            n' <- next is i n
            loop is (i+1) n'

newtype Day15 = D15 [Int]
instance Day Day15 where
    readDay _ = D15 . map read . splitOn ","
    part1 (D15 ns) = show $ runST $ run 2020 ns
    part2 (D15 ns) = show $ runST $ run 30000000 ns
