{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Day17.Vec
    ( Vec(..)
    , Vec3(..)
    , Vec4(..)
    , neighbors
    ) where

import Data.Bifunctor (first)
import Data.Bits (shiftL, shiftR, testBit, (.|.), (.&.))
import Data.Foldable (toList)
import Data.MemoTrie (HasTrie(..), memo)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

class Vec v where
    zero :: v
    add :: v -> v -> v
    invert :: v -> v
    basis :: [v]
    between :: v -> v -> [v]
    from2d :: Integral i => (i, i) -> v

neighbors :: (HasTrie v, Vec v) => v -> [v]
neighbors = memo (\v -> map (v `add`) $ tail $ deltas basis)
  where
    deltas [] = [zero]
    deltas (v:vs) = [ d `add` d' | d <- [zero, v, invert v], d' <- deltas vs ]

newtype Int5 = I5 Int
    deriving (Enum, Eq, Num, Ord)

data Vec3 = Vec3 !Int5 !Int5 !Int5
    deriving (Eq, Ord)
data Vec4 = Vec4 !Int5 !Int5 !Int5 !Int5
    deriving (Eq, Ord)

instance Vec Vec3 where
    zero = Vec3 0 0 0
    Vec3 x y z `add` Vec3 x' y' z' = Vec3 (x+x') (y+y') (z+z')
    invert (Vec3 x y z) = Vec3 (-x) (-y) (-z)
    basis = [Vec3 1 0 0, Vec3 0 1 0, Vec3 0 0 1]
    between (Vec3 x0 y0 z0) (Vec3 x1 y1 z1) =
        [ Vec3 x y z | z <- [z0..z1], y <- [y0..y1], x <- [x0..x1] ]
    from2d (x, y) = Vec3 (fromIntegral x) (fromIntegral y) 0

instance Vec Vec4 where
    zero = Vec4 0 0 0 0
    Vec4 x y z w `add` Vec4 x' y' z' w' = Vec4 (x+x') (y+y') (z+z') (w+w')
    invert (Vec4 x y z w) = Vec4 (-x) (-y) (-z) (-w)
    basis = [Vec4 1 0 0 0, Vec4 0 1 0 0, Vec4 0 0 1 0, Vec4 0 0 0 1]
    between (Vec4 x0 y0 z0 w0) (Vec4 x1 y1 z1 w1) =
        [ Vec4 x y z w | w <- [w0..w1], z <- [z0..z1], y <- [y0..y1], x <- [x0..x1] ]
    from2d (x, y) = Vec4 (fromIntegral x) (fromIntegral y) 0 0

widen :: Int5 -> Int
widen (I5 x) = x .&. 0x1f

narrow :: Int -> Int5
narrow x = I5 $ if testBit x 4 then (x .&. 0xf) - 0x10 else x .&. 0xf

pack3 :: Vec3 -> Int
pack3 (Vec3 x y z) = widen x .|. (widen y `shiftL` 5) .|. (widen z `shiftL` 10)

unpack3 :: Int -> Vec3
unpack3 a = Vec3 (narrow a) (narrow $ a `shiftR` 5) (narrow $ a `shiftR` 10)

pack4 :: Vec4 -> Int
pack4 (Vec4 x y z w) =
    widen x .|. (widen y `shiftL` 5) .|. (widen z `shiftL` 10) .|. (widen w `shiftL` 15)

unpack4 :: Int -> Vec4
unpack4 a = Vec4
    (narrow a) (narrow $ a `shiftR` 5) (narrow $ a `shiftR` 10) (narrow $ a `shiftR` 15)

instance HasTrie Vec3 where
    newtype (Vec3 :->: b) = Vec3Trie (Vector b)
    trie f = Vec3Trie $ V.generate (2^15) (f . unpack3)
    untrie (Vec3Trie t) = (t !) . pack3
    enumerate (Vec3Trie t) = zip (map unpack3 [0..]) (toList t)

instance HasTrie Vec4 where
    newtype (Vec4 :->: b) = Vec4Trie (Vector b)
    trie f = Vec4Trie $ V.generate (2^20) (f . unpack4)
    untrie (Vec4Trie t) = (t !) . pack4
    enumerate (Vec4Trie t) = zip (map unpack4 [0..]) (toList t)
