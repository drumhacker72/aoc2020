import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forM_, void, zipWithM)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)

import Calendar (days)
import Day (Day(Day))

benchDay :: (Int, Day) -> String -> IO (Int, UTCTime, UTCTime, UTCTime, UTCTime)
benchDay (n, Day readDay part1 part2) file = do
    t0 <- getCurrentTime
    let input = readDay file
    t1 <- getCurrentTime
    let (state, r1) = part1 input
    void $ evaluate $ force r1
    t2 <- getCurrentTime
    let r2 = part2 input state
    void $ evaluate $ force r2
    t3 <- getCurrentTime
    return (n, t0, t1, t2, t3)

main = do
    files <- mapM (\(n, _) -> readFile $ "input/Day" ++ show n ++ ".txt") days
    start <- getCurrentTime
    ts <- zipWithM benchDay days files
    end <- getCurrentTime
    forM_ ts $ \(n, t0, t1, t2, t3) -> do
        putStrLn $ "day " ++ show n ++ ": " ++ show (diffUTCTime t3 t0)
        putStrLn $ "\tinput:  " ++ show (diffUTCTime t1 t0)
        putStrLn $ "\tpart 1: " ++ show (diffUTCTime t2 t1)
        putStrLn $ "\tpart 2: " ++ show (diffUTCTime t3 t2)
    putStrLn $ "total: " ++ show (diffUTCTime end start)
