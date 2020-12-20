module Day20(Day20) where

import Control.Monad (guard, unless)
import Data.Char (isDigit)
import Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Sequence (Seq(Empty, (:<|)), (><))
import Text.ParserCombinators.ReadP ((+++))
import qualified Data.Sequence as S
import qualified Text.ParserCombinators.ReadP as P
import Day
import Debug.Trace

data Tile = Tile { tileId :: Int, tileData :: [[Char]] }
    deriving Show

row = P.munch1 $ \c -> c `elem` ".#"
tile = do
    P.string "Tile "
    n <- read <$> P.munch1 isDigit
    P.string ":\n"
    rows <- P.many1 (row <* P.char '\n')
    return $ Tile n rows
tiles = P.sepBy1 tile (P.char '\n')

readTiles s = case P.readP_to_S (tiles <* P.eof) s of [(ts, "")] -> ts

edges (Tile _ rows) =
    [ head rows -- 0 top
    , last cols -- 1 right
    , reverse $ last rows -- 2 bottom
    , reverse $ head cols -- 3 left
    ]
  where
    cols = transpose rows

rotEdges (tile, rot) = take 4 $ drop rot $ cycle $ edges tile
xformEdges (tile, rot, False) = rotEdges (tile, rot)
xformEdges (tile, rot, True) = reverse $ map reverse $ rotEdges (tile, rot)

first f (a, b, c) = (f a, b, c)

arrange :: Seq Tile -> [(Tile, Int, Bool)] -> [(Tile, Int, Bool)] -> [[(Tile, Int, Bool)]]
arrange Empty _ _ = return []
arrange tiles prevRow curRow = do
    i <- [0 .. length tiles - 1]
    let (left, tile :<| right) = S.splitAt i tiles
        restTiles = left >< right
    rot <- [0..3]
    flip <- [False, True]
    let edges = xformEdges (tile, rot, flip)
    unless (null prevRow) $ do
        let upEdges = xformEdges $ prevRow !! (11 - length curRow)
        guard $ (edges !! 0) == reverse (upEdges !! 2)
    unless (null curRow) $ do
        let leftEdges = xformEdges $ head curRow
        guard $ (edges !! 3) == reverse (leftEdges !! 1)
    let curRow' = (tile, rot, flip):curRow
    rest <- if length curRow' == 12
            then arrange restTiles curRow' []
            else arrange restTiles prevRow curRow'
    return $ (tile, rot, flip):rest

dropEdges = map (take 8 . drop 1) . take 8 . drop 1

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

extract z@(tile, _, _) = xform z $ tileData tile

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

newtype Day20 = D20 { runD20 :: [[(Tile, Int, Bool)]] }
instance Day Day20 where
    readDay _ s =
        let tiles = S.fromList $ readTiles s
            tiles' = head $ arrange tiles [] []
         in D20 $ chunksOf 12 tiles'
    part1 (D20 rows) =
        let x = map (map (tileId . (\(a,_,_) -> a))) rows
         in show $ (x !! 0 !! 0) * (x !! 11 !! 0) * (x !! 0 !! 11) * (x !! 11 !! 11)
    part2 (D20 rows) =
        let image = assemble $ map (map (dropEdges . extract)) $ rows
            seaMonsters = sum $ map (countMatches . ($ image)) xforms
            hashes = sum $ map (length . filter id . map (== '#')) image
         in show $ hashes - (seaMonsters * 15)
