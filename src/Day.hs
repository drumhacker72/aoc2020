module Day
    ( Day(..)
    ) where

import Data.Proxy (Proxy)

class Day a where
    readDay :: Proxy a -> String -> a
    part1 :: a -> String
    part2 :: a -> String
