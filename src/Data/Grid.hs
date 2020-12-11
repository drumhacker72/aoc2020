{-# LANGUAGE DeriveFunctor #-}

module Data.Grid
    ( Grid
    , up
    , down
    , left
    , right
    , toVector
    , fromVector
    ) where

import Control.Comonad (Comonad(..))
import Data.Vector (Vector, generate, (!))

data Grid a = Grid
    { row :: Int
    , col :: Int
    , empty :: a
    , values :: Vector (Vector a)
    } deriving Functor

instance Comonad Grid where
    extract (Grid r c e vs)
        | 0 <= r, r < length vs, 0 <= c, c < length (vs ! r) = vs ! r ! c
        | otherwise = e

    duplicate g@(Grid r c _ vs) = Grid r c undefined $
        generate (length vs) $ \r' ->
            generate (length $ vs ! r') $ \c' -> g{row = r', col = c'}

up g@Grid{row = r} = g{row = r-1}

down g@Grid{row = r} = g{row = r+1}

left g@Grid{col = c} = g{col = c-1}

right g@Grid{col = c} = g{col = c+1}

toVector Grid{values = vs} = vs

fromVector = Grid 0 0
