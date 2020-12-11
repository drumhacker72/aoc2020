{-# LANGUAGE DeriveFunctor #-}

module Data.Grid
    ( Grid(..)
    , up
    , down
    , left
    , right
    , emplace
    , modify
    , horizontal
    , vertical
    , toList
    , fromList
    ) where

import Control.Comonad (Comonad(..))
import Data.Zipper (Zipper)
import qualified Data.Zipper as Z

newtype Grid a = Grid { runGrid :: Zipper (Zipper a) }
    deriving Functor

instance Comonad Grid where
    extract = extract . extract . runGrid
    duplicate = Grid . fmap horizontal . vertical

up = Grid . Z.left . runGrid

down = Grid . Z.right . runGrid

left = Grid . fmap Z.left . runGrid

right = Grid . fmap Z.right . runGrid

emplace x = Grid . Z.modify (Z.emplace x)

modify f = Grid . Z.modify (Z.modify f)

horizontal = Z.move left right

vertical = Z.move up down

toList up down left right = map (Z.toList left right) . Z.toList up down . runGrid

fromList x = Grid . Z.fromList (Z.fromList x []) . map (Z.fromList x)
