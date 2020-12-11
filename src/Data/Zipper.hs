{-# LANGUAGE DeriveFunctor #-}

module Data.Zipper
    ( Zipper(..)
    , left
    , right
    , emplace
    , modify
    , toList
    , fromList
    , move
    ) where

import Control.Comonad (Comonad(..))

data Zipper a = Zipper [a] a [a]
    deriving Functor

instance Comonad Zipper where
    extract (Zipper _ x _) = x
    duplicate = move left right

left (Zipper (l:ls) x rs) = Zipper ls l (x:rs)

right (Zipper ls x (r:rs)) = Zipper (x:ls) r rs

emplace x (Zipper ls _ rs) = Zipper ls x rs

modify f (Zipper ls x rs) = Zipper ls (f x) rs

toList l r (Zipper ls x rs) = reverse (take l ls) ++ [x] ++ take r rs

fromList x [] = Zipper (repeat x) x (repeat x)
fromList x (a:as) = Zipper (repeat x) a (as ++ repeat x)

move f g z = Zipper (iterate f (f z)) z (iterate g (g z))
