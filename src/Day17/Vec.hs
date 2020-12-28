module Day17.Vec
    ( Vec(..)
    , Vec3
    , Vec4
    , neighbors
    ) where

import Data.Bits (shiftL, shiftR, (.|.))
import Data.Int (Int8)
import Data.Word (Word8)

class Vec v where
    zero :: v
    add :: v -> v -> v
    invert :: v -> v
    basis :: [v]
    from2d :: (Integral x, Integral y) => (x, y) -> v
    pack :: v -> Int
    unpack :: Int -> v

widen :: Int8 -> Int
widen x = fromIntegral (fromIntegral x :: Word8)
narrow :: Int -> Int8
narrow = fromIntegral

data Vec3 = Vec3 !Int8 !Int8 !Int8
    deriving (Eq, Ord)

instance Vec Vec3 where
    zero = Vec3 0 0 0
    Vec3 x y z `add` Vec3 x' y' z' = Vec3 (x+x') (y+y') (z+z')
    invert (Vec3 x y z) = Vec3 (-x) (-y) (-z)
    basis = [Vec3 1 0 0, Vec3 0 1 0, Vec3 0 0 1]
    from2d (x, y) = Vec3 (fromIntegral x) (fromIntegral y) 0
    pack (Vec3 x y z) = widen x .|. (widen y `shiftL` 8) .|. (widen z `shiftL` 16)
    unpack a = Vec3 (narrow a) (narrow $ a `shiftR` 8) (narrow $ a `shiftR` 16)

data Vec4 = Vec4 !Int8 !Int8 !Int8 !Int8
    deriving (Eq, Ord)

instance Vec Vec4 where
    zero = Vec4 0 0 0 0
    Vec4 x y z w `add` Vec4 x' y' z' w' = Vec4 (x+x') (y+y') (z+z') (w+w')
    invert (Vec4 x y z w) = Vec4 (-x) (-y) (-z) (-w)
    basis = [Vec4 1 0 0 0, Vec4 0 1 0 0, Vec4 0 0 1 0, Vec4 0 0 0 1]
    from2d (x, y) = Vec4 (fromIntegral x) (fromIntegral y) 0 0
    pack (Vec4 x y z w) = widen x .|. (widen y `shiftL` 8)
                          .|. (widen z `shiftL` 16) .|. (widen w `shiftL` 24)
    unpack a = Vec4 (narrow a) (narrow $ a `shiftR` 8)
                    (narrow $ a `shiftR` 16) (narrow $ a `shiftR` 24)

neighbors :: Vec v => v -> [v]
neighbors p = map (p `add`) $ tail $ deltas basis
  where
    deltas [] = [zero]
    deltas (v:vs) = [ d `add` d' | d <- [zero, v, invert v], d' <- deltas vs ]
