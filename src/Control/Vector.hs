module Control.Vector
    ( Vector(..)
    ) where

class Ord v => Vector v where
    zero :: v
    add :: v -> v -> v
    invert :: v -> v
    basis :: [v]
    from2d :: (Integral x, Integral y) => (x, y) -> v

instance (Integral x, Integral y, Integral z) => Vector (x, y, z) where
    zero = (0, 0, 0)
    (x, y, z) `add` (x', y', z') = (x+x', y+y', z+z')
    invert (x, y, z) = (-x, -y, -z)
    basis = [(1, 0, 0), (0, 1, 0), (0, 0, 1)]
    from2d (x, y) = (fromIntegral x, fromIntegral y, 0)

instance (Integral x, Integral y, Integral z, Integral w) => Vector (x, y, z, w) where
    zero = (0, 0, 0, 0)
    (x, y, z, w) `add` (x', y', z', w') = (x+x', y+y', z+z', w+w')
    invert (x, y, z, w) = (-x, -y, -z, -w)
    basis = [(1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0), (0, 0, 0, 1)]
    from2d (x, y) = (fromIntegral x, fromIntegral y, 0, 0)
