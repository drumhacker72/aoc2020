module Day13(Day13) where

import Data.List (minimumBy)
import Data.List.Split (splitWhen)
import Data.Maybe (catMaybes, mapMaybe)
import Day

readBus "x" = Nothing
readBus a = Just $ read a

timeUntil t bus = (-t) `mod` bus

-- | Find the smallest nonnegative value of a such that b is a nonnegative integer in:
-- t = a * m1 + c1 = b * m2 + c2
-- IIRC the Euclidean algorithm might be used to find this in fewer steps,
-- but simply bruteforcing it is fast enough.
findCoeff m1 c1 m2 c2 = findCoeff' 0
  where
    findCoeff' a =
        let (b, r) = (a * m1 + c1 - c2) `divMod` m2
         in if b >= 0 && r == 0 then a else findCoeff' (a+1)

-- | Take a system of two modular equivalences:
-- t ≡ c1 (mod m1)
-- t ≡ c2 (mod m2)
-- and simplify it into a singular modular equivalence:
-- t ≡ cr (mod mr)
-- with the same set of solutions for t.
--
-- For any valid t, there are unique integers a, b where:
-- t = a * m1 + c1, and t = b * m2 + c2
-- Given a valid t, t + m1*m2 is also a valid solution:
-- t + m1*m2 = a*m1 + c1 + m1*m2 = b*m2 + c2 + m1*m2
--           = (a+m2)*m1 + c1    = (b+m1)*m2 + c2
-- where a+m2, b+m1 are the new coeffecients for a, b that satisfy
-- the original modular equivalences.
-- So t ≡ a*m1 + c1 (mod m1*m2) and t ≡ b*m2 + c2 (mod m1*m2)
-- are each equally good replacements for the system.
--
-- Finding coeffecients a, b as the smallest nonnegative possible
-- pair will result in a*m1+c = b*m2+c2 equalling the smallest nonnegative t.
reduceMod (m1, c1) (m2, c2) =
    let a = findCoeff m1 c1 m2 c2
     in (m1 * m2, a * m1 + c1)

-- | Given (bus, index) to represent t+index ≡ 0 (mod bus),
-- invert to (bus, c) to represent t ≡ c (mod bus) for use in reduceMod.
invIndex (m, i) = (m, (-i) `mod` m)

newtype Day13 = D13 (Integer, [Maybe Integer])
instance Day Day13 where
    readDay _ s =
        let [l1, l2] = lines s
            t = read l1
            buses = map readBus $ splitWhen (== ',') l2
         in D13 (t, buses)
    part1 (D13 (t, buses)) =
        let busWaits = mapMaybe ((\b -> (b, timeUntil t b)) <$>) buses
            (bus, wait) = minimumBy (\(_, w1) (_, w2) -> compare w1 w2) busWaits
         in show $ bus * wait
    part2 (D13 (_, buses)) =
        let busWaits = catMaybes $ zipWith (\i -> ((\b -> (b, i)) <$>)) [0..] buses
            (_, t) = foldl1 reduceMod $ map invIndex busWaits
         in show t
