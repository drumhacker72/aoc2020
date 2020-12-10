module Day9(Day9) where

import Control.Monad (guard, (>=>))
import Data.List (find)
import Data.Sequence (Seq(Empty, (:<|)))
import qualified Data.Sequence as S
import Day

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

data Day9 = D9 (Seq Int) Int
instance Day Day9 where
    readDay _ input =
        let numbers = S.fromList $ map (read :: String -> Int) $ lines input
         in D9 numbers (firstInvalid numbers)
    part1 (D9 _ i) = show i
    part2 (D9 numbers i) =
        let Just range = findRange i numbers
         in show $ minimum range + maximum range
