module Day9 (day9) where

import Control.Monad (guard, (>=>))
import Data.List (find)
import Data.Sequence (Seq(Empty, (:<|)))
import qualified Data.Sequence as S
import Day (statefulDay)

readNumbers :: String -> Seq Int
readNumbers = S.fromList . map read . lines

candidates xs x = do
    a <- xs
    b <- xs
    guard $ a /= b && a + b == x

firstInvalid xs =
    let (preamble, x :<| _) = S.splitAt 25 xs
     in case candidates preamble x of
            Empty -> x
            _     -> firstInvalid $ S.drop 1 xs

ranges = S.inits >=> S.tails

findRange target = find ((== target) . sum) . S.filter ((>= 2) . length) . ranges

day9 = statefulDay readNumbers part1 part2
  where
    part1 numbers =
        let i = firstInvalid numbers
         in (i, show i)
    part2 numbers i =
        let Just range = findRange i numbers
         in show $ minimum range + maximum range
