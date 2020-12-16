{-# LANGUAGE TypeApplications #-}

import Data.Function ((&))
import Data.Proxy (Proxy(Proxy))
import System.Environment (getArgs)

import Day
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16

run t i = do
    let d = readDay t i
    putStrLn $ part1 d
    putStrLn $ part2 d

main = do
    n <- read . head <$> getArgs
    input <- readFile $ "input/Day" ++ show n ++ ".txt"
    input & case n of
        1 -> run $ Proxy @Day1
        2 -> run $ Proxy @Day2
        3 -> run $ Proxy @Day3
        4 -> run $ Proxy @Day4
        5 -> run $ Proxy @Day5
        6 -> run $ Proxy @Day6
        7 -> run $ Proxy @Day7
        8 -> run $ Proxy @Day8
        9 -> run $ Proxy @Day9
        10 -> run $ Proxy @Day10
        11 -> run $ Proxy @Day11
        12 -> run $ Proxy @Day12
        13 -> run $ Proxy @Day13
        14 -> run $ Proxy @Day14
        15 -> run $ Proxy @Day15
        16 -> run $ Proxy @Day16
