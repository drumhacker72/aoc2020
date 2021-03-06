{-# LANGUAGE TupleSections #-}

module Day13 (day13) where

import Data.Char (isDigit)
import Data.List (minimumBy)
import Data.Maybe (catMaybes, mapMaybe)
import Text.ParserCombinators.ReadP ((+++))
import qualified Text.ParserCombinators.ReadP as P
import Day (statelessDay)

bus = (P.char 'x' >> return Nothing) +++ (Just . read <$> P.munch1 isDigit)
busList = P.sepBy1 bus (P.char ',')
readBusList s = case P.readP_to_S (busList <* P.eof) s of [(bs, "")] -> bs

readInput s =
    let [l1, l2] = lines s
        t = read l1
        buses = readBusList l2
     in (t, buses)

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

day13 = statelessDay readInput part1 part2
  where
    part1 (t, buses) =
        let busWaits = mapMaybe ((\b -> (b, timeUntil t b)) <$>) buses
            (bus, wait) = minimumBy (\(_, w1) (_, w2) -> compare w1 w2) busWaits
         in show $ bus * wait
    part2 (_, buses) =
        let busWaits = catMaybes $ zipWith (\i -> ((, i) <$>)) [0..] buses
            (_, t) = foldl1 chineseRem $ map invIndex busWaits
         in show t
