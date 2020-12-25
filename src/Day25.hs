module Day25 (day25) where

import Control.Monad (foldM)
import Control.Monad.Trans.Reader (Reader, ask, runReader)
import Day (statelessDay)

readInput s =
    let [cardPub, doorPub] = read <$> lines s
     in (cardPub, doorPub)

step :: Int -> Reader Int Int
step v = do
    subjectNumber <- ask
    return $ v * subjectNumber `rem` 20201227

findLoopSize t@(target1, target2) loopSize v
    | v == target1 = Left loopSize
    | v == target2 = Right loopSize
    | otherwise    = findLoopSize t (loopSize+1) $ runReader (step v) 7

xform :: Int -> Reader Int Int
xform loopSize = foldM (const . step) 1 [1..loopSize]

day25 = statelessDay readInput part1 part2
  where
    part1 pub@(cardPub, doorPub) = show $ case findLoopSize pub 0 1 of
        Left cardLoopSize -> runReader (xform cardLoopSize) doorPub
        Right doorLoopSize -> runReader (xform doorLoopSize) cardPub
    part2 = const "free star"
