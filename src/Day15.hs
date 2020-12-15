{-# LANGUAGE FlexibleContexts #-}

module Day15(Day15) where

import Control.Monad (foldM, forM_)
import Control.Monad.ST (runST)
import Data.List.Split (splitOn)
import Data.Vector.Generic.Mutable (exchange, write)
import qualified Data.Vector.Unboxed.Mutable as VUM
import Day

next is n i = do
    prev <- exchange is n i
    return $ if prev == 0 then 0 else i - prev

run end ns = do
    is <- VUM.replicate end 0
    let (n, i):inits = reverse $ zip ns [1..]
    forM_ inits $ uncurry $ write is
    foldM (next is) n [i..end-1]

newtype Day15 = D15 [Int]
instance Day Day15 where
    readDay _ = D15 . map read . splitOn ","
    part1 (D15 ns) = show $ runST $ run 2020 ns
    part2 (D15 ns) = show $ runST $ run 30000000 ns
