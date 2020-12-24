module Day24 (day24) where

import Data.Map.Strict (Map, (!))
import Text.ParserCombinators.ReadP ((+++))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Text.ParserCombinators.ReadP as P
import Day (statefulDay)

(a, b) `add` (c, d) = (a+c, b+d)

east = P.char 'e' >> return (2, 0)
southeast = P.string "se" >> return (1, -1)
southwest = P.string "sw" >> return (-1, -1)
west = P.char 'w' >> return (-2, 0)
northwest = P.string "nw" >> return (-1, 1)
northeast = P.string "ne" >> return (1, 1)
direction = east +++ southeast +++ southwest +++ west +++ northwest +++ northeast
tile = P.chainl1 direction (return add)
readTile s = case P.readP_to_S (tile <* P.eof) s of [(d, "")] -> d
readTiles = map readTile . lines

adjacents = [(2, 0), (1, -1), (-1, -1), (-2, 0), (-1, 1), (1, 1)]

rule True blackAdjacents | blackAdjacents == 0 || blackAdjacents > 2 = False
rule False 2 = True
rule black _ = black

evolve :: Int -> Map (Int, Int) Bool -> Map (Int, Int) Bool
evolve 0 colors = colors
evolve i colors = evolve (i-1) (M.fromList $ map asdf cands')
  where
    cands = map fst $ filter snd $ M.toList colors
    cands' = S.toList $ S.unions $ map (\cand -> S.fromList $ map (\adj -> add cand adj) adjacents) cands
    asdf loc =
        let black = M.findWithDefault False loc colors
            blackAdjs = length $ filter id $ map ((\loc' -> M.findWithDefault False loc' colors) . (add loc)) adjacents
         in (loc, rule black blackAdjs)

day24 = statefulDay readTiles part1 part2
  where
    part1 locs =
        let asdf (Just True) = Just False
            asdf _ = Just True
            colors = foldr (M.alter asdf) M.empty locs
         in (colors, show $ length $ filter id $ M.elems colors)
    part2 _ colors =
        show $ length $ filter id $ M.elems $ evolve 100 colors
