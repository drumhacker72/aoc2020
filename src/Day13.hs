module Day13(Day13) where

import Data.List (elemIndex)
import Data.List.Split (splitWhen)
import Day

timeTo _ (-1) = 9999999
timeTo t b = -(t `mod` (-b))

match m1 c1 m2 c2 a =
    let (b, r) = (a * m1 + c1 - c2) `divMod` m2
     in if r == 0 && b >= 0 then a else match m1 c1 m2 c2 (a+1)

match' (m1, c1) (m2, c2) =
    let a = match m1 c1 m2 c2 0
     in (m1 * m2, a * m1 + c1)

fix "x" = (-1)
fix a = read a

newtype Day13 = D13 { runD13 :: (Integer, [Integer]) }
instance Day Day13 where
    readDay _ s =
        let [l1, l2] = lines s
            t = read l1
            bs = map fix $ splitWhen (== ',') l2 
         in D13 (t, bs)
    part1 (D13 (t, bs)) =
        let bs' = map (timeTo t) bs
            m = minimum bs'
            Just i = elemIndex m bs'
         in show $ (bs !! i) * (bs' !! i)
    part2 (D13 (_, bs)) =
        let m = foldl1 lcm (filter (/= -1) bs)
            bs' = foldl1 match' $ map (\(m,i) -> (m, (-i) `mod` m)) $ filter (\(m, _) -> m /= (-1)) $ zip bs [0..]
         in show $ snd bs' -- foldl1 lcm bs'
