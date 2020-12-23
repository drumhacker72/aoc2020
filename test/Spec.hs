import Control.Concurrent.ParallelIO (parallel, stopGlobalPool)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit
    ( Counts(errors, failures), Test(TestCase, TestLabel, TestList)
    , assertEqual, assertFailure, runTestTT
    )

import Calendar (days)
import Day (Day(Day))

test n exp1 exp2 = do
    file <- readFile $ "input/Day" ++ show n ++ ".txt"
    case lookup n days of
        Just (Day readDay part1 part2) -> do
            let input = readDay file
                (state, r1) = part1 input
                r2 = part2 input state
            (r1', r2') <- evaluate $ force (r1, r2)
            return (n, Just (exp1, exp2, r1', r2'))
        Nothing -> return (n, Nothing)

dayCases =
    [ test 1 "357504" "12747392"
    , test 2 "582" "729"
    , test 3 "207" "2655892800"
    , test 4 "245" "133"
    , test 5 "818" "559"
    , test 6 "6735" "3221"
    , test 7 "164" "7872"
    , test 8 "1521" "1016"
    , test 9 "258585477" "36981213"
    , test 10 "2475" "442136281481216"
    , test 11 "2338" "2134"
    , test 12 "1631" "58606"
    , test 13 "171" "539746751134958"
    , test 14 "14553106347726" "2737766154126"
    , test 15 "1259" "689"
    , test 16 "26941" "634796407951"
    , test 17 "448" "2400"
    , test 18 "53660285675207" "141993988282687"
    , test 19 "190" "311"
    , test 20 "14986175499719" "2161"
    , test 21 "2282" "vrzkz,zjsh,hphcb,mbdksj,vzzxl,ctmzsr,rkzqs,zmhnj"
    , test 22 "32401" "31436"
    , test 23 "38925764" "131152940564"
    ]

testDay (n, Nothing) = TestLabel ("day " ++ show n) $ TestCase $ assertFailure "unhandled"
testDay (n, Just (exp1, exp2, r1, r2)) = TestLabel ("day " ++ show n) $ TestList
    [ TestCase $ assertEqual "for part 1," exp1 r1
    , TestCase $ assertEqual "for part 2," exp2 r2
    ]

main = do
    xs <- parallel dayCases
    let tests = TestList $ map testDay xs
    results <- runTestTT tests
    stopGlobalPool
    if errors results + failures results == 0
    then exitSuccess
    else exitFailure
