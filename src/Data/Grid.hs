{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Data.Grid
    ( Grid2
    , Grid3
    , Grid4
    , Grid(..)
    , toList2
    , toList3
    , toList4
    ) where

import Control.Comonad (Comonad(..))
import Data.Zipper (Zipper)
import qualified Data.Zipper as Z

newtype ZipperX a = X { runX :: Zipper a }
    deriving (Comonad, Functor)
newtype ZipperY a = Y { runY :: Zipper a }
    deriving (Comonad, Functor)
newtype ZipperZ a = Z { runZ :: Zipper a }
    deriving (Comonad, Functor)
newtype ZipperW a = W { runW :: Zipper a }
    deriving (Comonad, Functor)

newtype Grid2 a = Grid2 { run2 :: ZipperY (ZipperX a) }
    deriving Functor
newtype Grid3 a = Grid3 { run3 :: ZipperZ (ZipperY (ZipperX a)) }
    deriving Functor
newtype Grid4 a = Grid4 { run4 :: ZipperW (ZipperZ (ZipperY (ZipperX a))) }
    deriving Functor

lift2 f = Grid2 . f . run2
lift3 f = Grid3 . f . run3
lift4 f = Grid4 . f . run4

liftX f = X . f . runX
liftY f = Y . f . runY
liftZ f = Z . f . runZ
liftW f = W . f . runW

dupWith lift = Z.move (lift Z.left) (lift Z.right)

instance Comonad Grid2 where
    extract = extract . extract . run2
    duplicate =
        let y = Y . dupWith (lift2 . liftY)
            x = fmap $ X . dupWith (lift2 . fmap . liftX)
         in Grid2 . x . y
instance Comonad Grid3 where
    extract = extract . extract . extract . run3
    duplicate =
        let z = Z . dupWith (lift3 . liftZ)
            y = fmap $ Y . dupWith (lift3 . fmap . liftY)
            x = fmap . fmap $ X . dupWith (lift3 . fmap . fmap . liftX)
         in Grid3 . x . y . z
instance Comonad Grid4 where
    extract = extract . extract . extract . extract . run4
    duplicate =
        let w = W . dupWith (lift4 . liftW)
            z = fmap $ Z . dupWith (lift4 . fmap . liftZ)
            y = fmap . fmap $ Y . dupWith (lift4 . fmap . fmap . liftY)
            x = fmap . fmap . fmap $ X . dupWith (lift4 . fmap . fmap . fmap . liftX)
         in Grid4 . x . y . z . w

fillX e = X $ Z.fromList e []
fillY e = Y $ Z.fromList (fillX e) []
fillZ e = Z $ Z.fromList (fillY e) []

class Grid g a where
    neighbors :: g a -> [g a]
    fromSlice :: a -> [[a]] -> g a
instance Grid Grid2 a where
    neighbors g = tail
        [ g''
        | dy <- [id, Z.left, Z.right]
        , let g' = (lift2 . liftY) dy g
        , dx <- [id, Z.left, Z.right]
        , let g'' = (lift2 . fmap . liftX) dx g'
        ]
    fromSlice e rows =
        let x = X . Z.fromList e
            y = Y $ Z.fromList (fillX e) $ map x rows
         in Grid2 y
instance Grid Grid3 a where
    neighbors g = tail
        [ g'''
        | dz <- [id, Z.left, Z.right]
        , let g' = (lift3 . liftZ) dz g
        , dy <- [id, Z.left, Z.right]
        , let g'' = (lift3 . fmap . liftY) dy g'
        , dx <- [id, Z.left, Z.right]
        , let g''' = (lift3 . fmap . fmap . liftX) dx g''
        ]
    fromSlice e rows =
        let x = X . Z.fromList e
            y = Y $ Z.fromList (fillX e) $ map x rows
            z = Z $ Z.fromList (fillY e) [y]
         in Grid3 z
instance Grid Grid4 a where
    neighbors g = tail
        [ g''''
        | dw <- [id, Z.left, Z.right]
        , let g' = (lift4 . liftW) dw g
        , dz <- [id, Z.left, Z.right]
        , let g'' = (lift4 . fmap . liftZ) dz g'
        , dy <- [id, Z.left, Z.right]
        , let g''' = (lift4 . fmap . fmap . liftY) dy g''
        , dx <- [id, Z.left, Z.right]
        , let g'''' = (lift4 . fmap . fmap . fmap . liftX) dx g'''
        ]
    fromSlice e rows =
        let x = X . Z.fromList e
            y = Y $ Z.fromList (fillX e) $ map x rows
            z = Z $ Z.fromList (fillY e) [y]
            w = W $ Z.fromList (fillZ e) [z]
         in Grid4 w

toList2 :: (Int, Int) -> (Int, Int) -> Grid2 a -> [[a]]
toList2 (x0, y0) (x1, y1) =
    let x = Z.toList x0 x1 . runX
        y = map x . Z.toList y0 y1 . runY
     in y . run2
toList3 :: (Int, Int, Int) -> (Int, Int, Int) -> Grid3 a -> [[[a]]]
toList3 (x0, y0, z0) (x1, y1, z1) =
    let x = Z.toList x0 x1 . runX
        y = map x . Z.toList y0 y1 . runY
        z = map y . Z.toList z0 z1 . runZ
     in z . run3
toList4 :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Grid4 a -> [[[[a]]]]
toList4 (x0, y0, z0, w0) (x1, y1, z1, w1) =
    let x = Z.toList x0 x1 . runX
        y = map x . Z.toList y0 y1 . runY
        z = map y . Z.toList z0 z1 . runZ
        w = map z . Z.toList w0 w1 . runW
     in w . run4
