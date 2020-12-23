module Day23 (day23) where

import Control.Monad (zipWithM_)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (runST)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Char (digitToInt)
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as V
import Day (statelessDay)

type Cup = Int
readCups :: String -> [Cup]
readCups s = map digitToInt line
  where
    [line] = lines s

type Circle m = MVector (PrimState m) Cup
mkCircle :: PrimMonad m => [Cup] -> m (Circle m)
mkCircle cups = do
    circle <- V.unsafeNew $ length cups + 1
    zipWithM_ (V.write circle) cups (tail cups ++ [head cups])
    return circle

newtype MaxCup = MaxCup Cup

destination :: Monad m => [Cup] -> Cup -> ReaderT MaxCup m Cup
destination pickup c
    | c < 1           = ask >>= \(MaxCup c') -> destination pickup c'
    | c `elem` pickup = destination pickup (c-1)
    | otherwise       = return c

move :: PrimMonad m => Cup -> Circle m -> ReaderT MaxCup m Cup
move current circle = do
    pickup <- takeFrom 3 current circle
    afterPickup <- V.read circle (last pickup)
    dest <- destination pickup (current-1)
    afterDest <- V.read circle dest
    V.write circle current afterPickup
    V.write circle dest (head pickup)
    V.write circle (last pickup) afterDest
    V.read circle current

run :: PrimMonad m => Int -> Circle m -> Cup -> ReaderT MaxCup m ()
run 0 _      _       = return ()
run i circle current = move current circle >>= run (i-1) circle

takeFrom :: PrimMonad m => Int -> Cup -> Circle m -> m [Cup]
takeFrom 0     _   _      = return []
takeFrom count cup circle = do
    next <- V.read circle cup
    rest <- takeFrom (count-1) next circle
    return (next:rest)

day23 = statelessDay readCups part1 part2
  where
    part1 cups = concatMap show $ runST $ do
        let maxCup = length cups
        circle <- mkCircle cups
        runReaderT (run 100 circle (head cups)) (MaxCup maxCup)
        takeFrom (length cups - 1) 1 circle
    part2 cups = show $ product $ runST $ do
        let cups' = cups ++ [length cups + 1 .. 1000000]
        circle <- mkCircle cups'
        runReaderT (run 10000000 circle (head cups')) (MaxCup 1000000)
        takeFrom 2 1 circle
