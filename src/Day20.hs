{-# LANGUAGE FlexibleContexts #-}

module Day20 (day20) where

import Control.Monad (guard, unless)
import Data.Array.Repa (Array, D, DIM2, Source, U, Z(Z), (:.)((:.)))
import Data.Char (isDigit)
import Data.List (transpose)
import Data.Sequence (Seq(Empty, (:<|), (:|>)), (><))
import Text.ParserCombinators.ReadP ((+++))
import qualified Data.Array.Repa as R
import qualified Data.Sequence as S
import qualified Text.ParserCombinators.ReadP as P
import Day (statefulDay)

type Image r = Array r DIM2 Char

data Tile = Tile { tileId :: Int, tileImage :: Image U }

row = P.munch1 $ \c -> c `elem` ".#"
tile = do
    P.string "Tile "
    n <- read <$> P.munch1 isDigit
    P.string ":\n"
    rows <- P.sepBy1 row (P.char '\n')
    return $ Tile n $ arrayFromRows rows
tiles = P.sepBy1 tile (P.string "\n\n")

readTiles s = case P.readP_to_S (tiles <* P.skipSpaces <* P.eof) s of [(ts, "")] -> ts

arrayFromRows rows = R.fromListUnboxed (Z :. length rows :. length (head rows)) (concat rows)

width image = case R.extent image of Z :. _ :. w -> w
height image = case R.extent image of Z :. h :. _ -> h

edges image =
    [ R.slice image (Z :. (0::Int) :. R.All)         -- 0 top
    , R.slice image (Z :. R.All :. width image - 1)  -- 1 right
    , R.slice image (Z :. height image - 1 :. R.All) -- 2 bottom
    , R.slice image (Z :. R.All :. (0::Int))         -- 3 left
    ]

top = (!! 0)
right = (!! 1)
bottom = (!! 2)
left = (!! 3)

flipX :: Source r Char => Image r -> Image D
flipX image = R.backpermute e swap image
  where
    e@(Z :. w :. _) = R.extent image
    swap (Z :. x :. y) = Z :. (w-1 - x) :. y
flipY :: Source r Char => Image r -> Image D
flipY image = R.backpermute e swap image
  where
    e@(Z :. _ :. h) = R.extent image
    swap (Z :. x :. y) = Z :. x :. (h-1 - y)

xforms :: Source r Char => Image r -> [Image D]
xforms image = map ($ image) $ do
    fx <- [R.delay, flipX]
    fy <- [R.delay, flipY]
    tp <- [R.delay, R.transpose]
    return $ tp . fy . fx

arrangements :: Int -> Seq Tile -> Seq (Image D) -> Seq (Image D) -> [[Tile]]
arrangements _ Empty _ _ = return []
arrangements w tiles prevRow curRow = do
    i <- [0 .. length tiles - 1]
    let (leftTiles, (Tile tid origImage) :<| rightTiles) = S.splitAt i tiles
        restTiles = leftTiles >< rightTiles
    image <- xforms origImage
    let es = edges image
    unless (null prevRow) $ do
        let u = prevRow `S.index` length curRow
            upEdges = edges u
        guard $ top es == bottom upEdges
    unless (null curRow) $ do
        let _ :|> l = curRow
            leftEdges = edges l
        guard $ left es == right leftEdges
    let curRow' = curRow :|> image
    rest <- if length curRow' == w
            then arrangements w restTiles curRow' Empty
            else arrangements w restTiles prevRow curRow'
    let image' = R.computeS image
    return $ Tile tid image':rest

dropEdges image = R.extract (Z :. (1::Int) :. (1::Int)) (Z :. h-2 :. w-2) image
  where
    (Z :. h :. w) = R.extent image

assembleRow :: [Image D] -> Image D
assembleRow = foldl1 R.append
assemble :: [[Image D]] -> Image D
assemble = R.transpose . assembleRow . map (R.transpose . assembleRow)

chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

extract = dropEdges . tileImage

seaMonster = arrayFromRows
    [ "                  # "
    , "#    ##    ##    ###"
    , " #  #  #  #  #  #   "
    ]

charMatch mask src = mask == ' ' || src == mask

matchesAt slice | R.extent slice == R.extent seaMonster =
    R.foldAllS (&&) True $ R.zipWith charMatch seaMonster slice

countMatches :: Image D -> Int
countMatches = \image -> R.sumAllS $ R.traverse image f g
  where
    f (Z :. h :. w) = Z :. (h-height seaMonster) :. (w-width seaMonster)
    g get (Z :. y :. x) =
        let get' (Z :. y' :. x') = get (Z :. y'+y :. x'+x)
            slice = R.fromFunction (R.extent seaMonster) get'
         in if matchesAt slice then 1 else 0

day20 = statefulDay readTiles part1 part2
  where
    part1 tiles =
        let w = round $ sqrt $ fromIntegral $ length tiles
            tiles' = head $ arrangements w (S.fromList tiles) Empty Empty
            rows = chunksOf w tiles'
            x = map (map tileId) rows
            top = head x
            bottom = last x
         in (rows, show $ head top * last top * head bottom * last bottom)
    part2 _ rows =
        let image = R.computeUnboxedS $ assemble $ map (map extract) rows
            seaMonsters = sum $ map countMatches $ xforms image
            hashes = R.sumAllS $ R.map (\c -> if c == '#' then 1 else 0) image
         in show $ hashes - (seaMonsters * 15)
