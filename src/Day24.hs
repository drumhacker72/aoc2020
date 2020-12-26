module Day24 (day24) where

import Data.Map.Strict (Map)
import Text.ParserCombinators.ReadP (ReadP, (+++))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Text.ParserCombinators.ReadP as P
import Day (statefulDay)
import Day24.BoolGrid (BoolGrid)
import qualified Day24.BoolGrid as BG

e = P.char 'e' >> return east
se = P.string "se" >> return southeast
sw = P.string "sw" >> return southwest
w = P.char 'w' >> return west
nw = P.string "nw" >> return northwest
ne = P.string "ne" >> return northeast
direction :: ReadP ((Int, Int) -> (Int, Int))
direction = e +++ se +++ sw +++ w +++ nw +++ ne
tile = ($ (0, 0)) . foldr1 (.) <$> P.many1 direction
readTile s = case P.readP_to_S (tile <* P.eof) s of [(d, "")] -> d
readTiles = map readTile . lines

east      (x,p) = (x+1,p  )
southeast (x,p) = (x+1,p-1)
southwest (x,p) = (x  ,p-1)
west      (x,p) = (x-1,p  )
northwest (x,p) = (x-1,p+1)
northeast (x,p) = (x  ,p+1)

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors xp = map ($ xp) [east, southeast, southwest, west, northwest, northeast]

candidates grid =
    let ps = BG.elems grid
        ns = map (S.fromList . neighbors) $ S.toList ps
     in S.toList $ S.unions (ps:ns)

evolve grid = BG.fromList $ map (\p -> (p, evolve' p (BG.get p grid))) $ candidates grid
  where
    evolve' p v =
        let n = length $ filter id $ map (`BG.get` grid) $ neighbors p
         in case (v, n) of
             (True,  0) -> False
             (True,  n) | n > 2 -> False
             (False, 2) -> True
             (color, _) -> color

day24 = statefulDay readTiles part1 part2
  where
    part1 tiles =
        let flipColor (Just True) = Just False
            flipColor _           = Just True
            colors = BG.fromList $ M.toList $ foldr (M.alter flipColor) M.empty tiles
         in (colors, show $ BG.count colors)
    part2 tiles colors = show . BG.count $ iterate evolve colors !! 100
