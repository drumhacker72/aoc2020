{-# LANGUAGE TypeApplications #-}

import Control.Concurrent.ParallelIO (parallel, stopGlobalPool)
import Data.Proxy (Proxy(Proxy))
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit
    ( Counts(errors, failures), Test(TestCase, TestLabel, TestList)
    , assertEqual, runTestTT
    )

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
import Day17
import Day18
import Day19
import Day20

day t exp1 exp2 n = do
    input <- readFile $ "input/Day" ++ show n ++ ".txt"
    let input' = readDay t input
    [r1, r2] <- parallel [return $! part1 input', return $! part2 input']
    return ((exp1, exp2), (r1, r2))

days =
    [ day (Proxy @Day1) "357504" "12747392"
    , day (Proxy @Day2) "582" "729"
    , day (Proxy @Day3) "207" "2655892800"
    , day (Proxy @Day4) "245" "133"
    , day (Proxy @Day5) "818" "559"
    , day (Proxy @Day6) "6735" "3221"
    , day (Proxy @Day7) "164" "7872"
    , day (Proxy @Day8) "1521" "1016"
    , day (Proxy @Day9) "258585477" "36981213"
    , day (Proxy @Day10) "2475" "442136281481216"
    , day (Proxy @Day11) "2338" "2134"
    , day (Proxy @Day12) "1631" "58606"
    , day (Proxy @Day13) "171" "539746751134958"
    , day (Proxy @Day14) "14553106347726" "2737766154126"
    , day (Proxy @Day15) "1259" "689"
    , day (Proxy @Day16) "26941" "634796407951"
    , day (Proxy @Day17) "448" "2400"
    , day (Proxy @Day18) "53660285675207" "141993988282687"
    , day (Proxy @Day19) "190" "311"
    , day (Proxy @Day20) "14986175499719" "2161"
    ]

testDay ((exp1, exp2), (r1, r2)) n = TestLabel ("day " ++ show n) $ TestList
    [ TestCase $ assertEqual "for part 1," exp1 r1
    , TestCase $ assertEqual "for part 2," exp2 r2
    ]

main = do
    xs <- parallel $ zipWith ($!) days [1..]
    let tests = TestList $ zipWith testDay xs [1..]
    results <- runTestTT tests
    stopGlobalPool
    if errors results + failures results == 0
    then exitSuccess
    else exitFailure
