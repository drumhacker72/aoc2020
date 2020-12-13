module Day13(Day13) where

import Data.List (minimumBy)
import Data.List.Split (splitWhen)
import Data.Maybe (catMaybes, mapMaybe)
import Day

readBus "x" = Nothing
readBus a = Just $ read a

timeUntil t bus = (-t) `mod` bus

-- | Find Bézout coefficients (a, b) such that
-- a * m1 + b * m2 = gcd m1 m2
-- using the extended Euclidean algorithm.
bezout m1 m2 = run extEuclid
  where
    extEuclid = (m1, 1, 0) : (m2, 0, 1) : zipWith next extEuclid (drop 1 extEuclid)
    next (r0, s0, t0) (r1, s1, t1) = (r2, s2, t2)
      where
        (q, r2) = r0 `quotRem` r1
        s2 = s0 - q * s1
        t2 = t0 - q * t1
    run ((_, a, b):(0, _, _):_) = (a, b)
    run (_:rest) = run rest

-- | Apply the Chinese remainder theorem to reduce
-- two modular congruences:
-- t ≡ c1 (mod m1)
-- t ≡ c2 (mod m2)
-- to a singular congruence:
-- t ≡ cr (mod mr)
chineseRem (m1, c1) (m2, c2)
    | gcd m1 m2 /= 1 = error "moduli are assumed to be coprime"
    | otherwise =
        let (a, b) = bezout m1 m2
         in (m1*m2, (c1*m2*b + c2*m1*a) `mod` (m1*m2))

-- | Given (bus, index) to represent t+index ≡ 0 (mod bus),
-- invert to (bus, c) to represent t ≡ c (mod bus) for use in chineseRem.
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
            (_, t) = foldl1 chineseRem $ map invIndex busWaits
         in show t
