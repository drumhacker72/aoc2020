{-# LANGUAGE FlexibleContexts #-}

module Day20(Day20) where

import Control.Monad (guard, unless)
import Data.Array.Repa (Array, D, DIM2, Source, U, Z(Z), (:.)((:.)))
import Data.Char (isDigit)
import Data.Function ((&))
import Data.List (transpose)
import Data.Sequence (Seq(Empty, (:<|), (:|>)), (><))
import Text.ParserCombinators.ReadP ((+++))
import qualified Data.Array.Repa as R
import qualified Data.Sequence as S
import qualified Text.ParserCombinators.ReadP as P
import Day

type Image r = Array r DIM2 Char

data Tile = Tile { tileId :: Int, tileImage :: Image U }

row = P.munch1 $ \c -> c `elem` ".#"
tile = do
    P.string "Tile "
    n <- read <$> P.munch1 isDigit
    P.string ":\n"
    rows <- P.sepBy1 row (P.char '\n')
    return $ Tile n $ R.fromListUnboxed (Z :. length rows :. length (head rows)) (concat rows)
tiles = P.sepBy1 tile (P.string "\n\n")

readTiles s = case P.readP_to_S (tiles <* P.skipSpaces <* P.eof) s of [(ts, "")] -> ts

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
iden :: Source r Char => Image r -> Image D
iden = R.map id

xform :: Source r Char => Bool -> Bool -> Bool -> Image r -> Image D
xform fx fy tp image = image
    & (if fx then flipX else iden)
    & (if fy then flipY else iden)
    & (if tp then R.transpose else iden)

arrangements :: Int -> Seq Tile -> Seq (Image D) -> Seq (Image D) -> [[Tile]]
arrangements _ Empty _ _ = return []
arrangements w tiles prevRow curRow = do
    i <- [0 .. length tiles - 1]
    let (leftTiles, (Tile tid origImage) :<| rightTiles) = S.splitAt i tiles
        restTiles = leftTiles >< rightTiles
    fx <- [False, True]
    fy <- [False, True]
    tp <- [False, True]
    let image = xform fx fy tp origImage
        es = edges image
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

xforms = do
    a <- [iden, flipX]
    b <- [iden, flipY]
    c <- [iden, R.transpose]
    return $ a . b . c

chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

extract = dropEdges . tileImage

seaMonster = R.fromListUnboxed (Z :. (3::Int) :. (20::Int)) $ concat
    [ "                  # "
    , "#    ##    ##    ###"
    , " #  #  #  #  #  #   "
    ]

charMatch mask src = mask == ' ' || src == mask

matchesAt = R.foldAllS (&&) True . R.zipWith charMatch seaMonster

countMatches :: Int -> Int -> Image D -> Int
countMatches x y image
    | x+20 > w  = countMatches 0 (y+1) image
    | y+3 > h   = 0
    | otherwise =
        let slice = R.extract (Z :. y :. x) (Z :. (3::Int) :. (20::Int)) image
            n = if matchesAt slice then 1 else 0
         in n + countMatches (x+1) y image
  where
    e@(Z :. h :. w) = R.extent image

data Day20 = D20 Int [[Tile]]
instance Day Day20 where
    readDay _ s =
        let w = round $ sqrt $ fromIntegral $ length tiles
            tiles = S.fromList $ readTiles s
            tiles' = head $ arrangements w tiles Empty Empty
         in D20 w $ chunksOf w tiles'
    part1 (D20 w rows) =
        let x = map (map tileId) rows
         in show $ head (head x) * last (head x) * head (last x) * last (last x)
    part2 (D20 _ rows) =
        let image = R.computeUnboxedS $ assemble $ map (map extract) rows
            seaMonsters = sum $ map (countMatches 0 0 . ($ image)) xforms
            hashes = R.sumAllS $ R.map (\c -> if c == '#' then 1 else 0) image
         in show $ hashes - (seaMonsters * 15)
