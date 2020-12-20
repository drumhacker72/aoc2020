{-# LANGUAGE TypeApplications #-}

import Data.Proxy (Proxy(Proxy))
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit (Counts(errors, failures), Test(TestCase, TestList), assertEqual, runTestTT)

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

assertDay n t p1 p2 = do
    input <- readFile $ "input/Day" ++ show n ++ ".txt"
    let d = readDay t input
    assertEqual ("for day " ++ show n ++ ", part 1,") p1 (part1 d)
    assertEqual ("for day " ++ show n ++ ", part 2,") p2 (part2 d)

tests = TestList
    [ TestCase $ assertDay 1 (Proxy @Day1) "357504" "12747392"
    , TestCase $ assertDay 2 (Proxy @Day2) "582" "729"
    , TestCase $ assertDay 3 (Proxy @Day3) "207" "2655892800"
    , TestCase $ assertDay 4 (Proxy @Day4) "245" "133"
    , TestCase $ assertDay 5 (Proxy @Day5) "818" "559"
    , TestCase $ assertDay 6 (Proxy @Day6) "6735" "3221"
    , TestCase $ assertDay 7 (Proxy @Day7) "164" "7872"
    , TestCase $ assertDay 8 (Proxy @Day8) "1521" "1016"
    , TestCase $ assertDay 9 (Proxy @Day9) "258585477" "36981213"
    , TestCase $ assertDay 10 (Proxy @Day10) "2475" "442136281481216"
    , TestCase $ assertDay 11 (Proxy @Day11) "2338" "2134"
    , TestCase $ assertDay 12 (Proxy @Day12) "1631" "58606"
    , TestCase $ assertDay 13 (Proxy @Day13) "171" "539746751134958"
    , TestCase $ assertDay 14 (Proxy @Day14) "14553106347726" "2737766154126"
    , TestCase $ assertDay 15 (Proxy @Day15) "1259" "689"
    , TestCase $ assertDay 16 (Proxy @Day16) "26941" "634796407951"
    , TestCase $ assertDay 17 (Proxy @Day17) "448" "2400"
    , TestCase $ assertDay 18 (Proxy @Day18) "53660285675207" "141993988282687"
    , TestCase $ assertDay 19 (Proxy @Day19) "190" "311"
    , TestCase $ assertDay 20 (Proxy @Day20) "14986175499719" "2161"
    ]

main = do
    results <- runTestTT tests
    if errors results + failures results == 0
    then exitSuccess
    else exitFailure
