import System.Environment (getArgs)

import Calendar (days)
import Day (Day(Day))

run file (Day readDay part1 part2) = do
    let input = readDay file
        (state, r1) = part1 input
        r2 = part2 input state
    putStrLn r1
    putStrLn r2

main = do
    [n] <- getArgs
    let n' = read n
    file <- readFile $ "input/Day" ++ show n' ++ ".txt"
    maybe (putStrLn "bad day") (run file) $ lookup n' days
