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
    ]

main = do
    results <- runTestTT tests
    if errors results + failures results == 0
    then exitSuccess
    else exitFailure
