{-# LANGUAGE FlexibleContexts #-}

module Day20(Day20) where

import Control.Monad (guard, unless)
import Data.Array.Repa (Array, DIM2, U, Z(Z), (:.)((:.)))
import Data.Char (isDigit)
import Data.List (transpose)
import Data.Sequence (Seq(Empty, (:<|)), (><))
import Text.ParserCombinators.ReadP ((+++))
import qualified Data.Array.Repa as R
import qualified Data.Sequence as S
import qualified Text.ParserCombinators.ReadP as P
import Day

type Image = Array U DIM2 Char

data Tile = Tile { tileId :: Int, tileImage :: Image }

row = P.munch1 $ \c -> c `elem` ".#"
tile = do
    P.string "Tile "
    n <- read <$> P.munch1 isDigit
    P.string ":\n"
    rows <- P.many1 (row <* P.char '\n')
    return $ Tile n $ R.fromListUnboxed (Z :. length rows :. length (head rows)) (concat rows)
tiles = P.sepBy1 tile (P.char '\n')

readTiles s = case P.readP_to_S (tiles <* P.eof) s of [(ts, "")] -> ts

width image = case R.extent image of Z :. _ :. w -> w
height image = case R.extent image of Z :. h :. _ -> h

rev slice = R.backpermute e swap slice
  where
    e@(Z :. w) = R.extent slice
    swap (Z :. i) = Z :. (w-1 - i)

edges (Tile _ image) =
    [ R.slice image (Z :. (0::Int) :. R.All)               -- 0 top
    , R.slice image (Z :. R.All :. width image - 1)        -- 1 right
    , rev $ R.slice image (Z :. height image - 1 :. R.All) -- 2 bottom
    , rev $ R.slice image (Z :. R.All :. (0::Int))         -- 3 left
    ]

rotEdges (tile, rot) = take 4 $ drop rot $ cycle $ edges tile
xformEdges (tile, rot, False) = rotEdges (tile, rot)
xformEdges (tile, rot, True) = reverse $ map rev $ rotEdges (tile, rot)

first f (a, b, c) = (f a, b, c)

arrange :: Int -> Seq Tile -> [(Tile, Int, Bool)] -> [(Tile, Int, Bool)] -> [[(Tile, Int, Bool)]]
arrange _ Empty _ _ = return []
arrange w tiles prevRow curRow = do
    i <- [0 .. length tiles - 1]
    let (left, tile :<| right) = S.splitAt i tiles
        restTiles = left >< right
    rot <- [0..3]
    flip <- [False, True]
    let edges = xformEdges (tile, rot, flip)
    unless (null prevRow) $ do
        let upEdges = xformEdges $ prevRow !! (w-1 - length curRow)
        guard $ (edges !! 0) == rev (upEdges !! 2)
    unless (null curRow) $ do
        let leftEdges = xformEdges $ head curRow
        guard $ (edges !! 3) == rev (leftEdges !! 1)
    let curRow' = (tile, rot, flip):curRow
    rest <- if length curRow' == w
            then arrange w restTiles curRow' []
            else arrange w restTiles prevRow curRow'
    return $ (tile, rot, flip):rest

dropEdges image = R.extract (Z :. (1::Int) :. (1::Int)) (Z :. h-2 :. w-2) image
  where
    (Z :. h :. w) = R.extent image

assembleRow = foldr1 (zipWith (++))
assemble = concatMap assembleRow

xform (_, 0, False) = id
xform (_, 0, True)  = transpose
xform (_, 1, False) = reverse . transpose
xform (_, 1, True)  = map reverse
xform (_, 2, False) = reverse . map reverse
xform (_, 2, True)  = reverse . map reverse . transpose
xform (_, 3, False) = map reverse . transpose
xform (_, 3, True)  = reverse

xforms = do
    a <- [id, reverse]
    b <- [id, map reverse]
    c <- [id, transpose]
    return $ a . b . c

chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

extract z@(tile, _, _) = xform z $ chunksOf w $ R.toList trimmed
  where
    trimmed = dropEdges $ tileImage tile
    (Z :. _ :. w) = R.extent trimmed

seaMonster =
    [ "                  # "
    , "#    ##    ##    ###"
    , " #  #  #  #  #  #   "
    ]

charMatch src p = src == p || p == ' '

matchesAt rows = and $ zipWith (\a b -> and $ zipWith charMatch a b) rows seaMonster

countMatches :: [[Char]] -> Int
countMatches rows
    | length rows < 3 = 0
    | otherwise =
        let n = length $ filter id $ map (\i -> matchesAt $ map (drop i) rows) [0 .. length (head rows) - 20]
         in n + countMatches (drop 1 rows)

data Day20 = D20 Int [[(Tile, Int, Bool)]]
instance Day Day20 where
    readDay _ s =
        let w = round $ sqrt $ fromIntegral $ length tiles
            tiles = S.fromList $ readTiles s
            tiles' = head $ arrange w tiles [] []
         in D20 w $ chunksOf w tiles'
    part1 (D20 w rows) =
        let x = map (map (tileId . (\(a,_,_) -> a))) rows
         in show $ (x !! 0 !! 0) * (x !! (w-1) !! 0) * (x !! 0 !! (w-1)) * (x !! (w-1) !! (w-1))
    part2 (D20 _ rows) =
        let image = assemble $ map (map extract) rows
            seaMonsters = sum $ map (countMatches . ($ image)) xforms
            hashes = sum $ map (length . filter id . map (== '#')) image
         in show $ hashes - (seaMonsters * 15)
