{-# LANGUAGE ExistentialQuantification #-}

module Day
    ( Day(..)
    , statefulDay
    , statelessDay
    ) where

data Day = forall i s. Day
    { _readDay :: String -> i
    , _part1 :: i -> (s, String)
    , _part2 :: i -> s -> String
    }

statefulDay :: (String -> i) -> (i -> (s, String)) -> (i -> s -> String) -> Day
statefulDay = Day

statelessDay :: (String -> i) -> (i -> String) -> (i -> String) -> Day
statelessDay readDay part1 part2 = Day
    readDay
    (\i -> ((), part1 i))
    (\i _ -> part2 i)
