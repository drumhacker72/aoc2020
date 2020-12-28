module Day17.BoolGrid
    ( BoolGrid
    , mkGrid
    , get
    , set
    , elems
    , count
    , fromList
    , from2dSlice
    ) where

import Data.IntSet (IntSet)
import Data.Set (Set)
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Day17.Vec (Vec(add, basis, from2d, invert, pack, unpack, zero))

newtype BoolGrid v = BoolGrid IntSet

mkGrid :: Vec v => BoolGrid v
mkGrid = BoolGrid IS.empty

get :: Vec v => v -> BoolGrid v -> Bool
get v (BoolGrid s) = pack v `IS.member` s

set :: Vec v => v -> Bool -> BoolGrid v -> BoolGrid v
set v False (BoolGrid s) = BoolGrid $ IS.delete (pack v) s
set v True  (BoolGrid s) = BoolGrid $ IS.insert (pack v) s

elems :: (Ord v, Vec v) => BoolGrid v -> Set v
elems (BoolGrid s) = S.fromList $ map unpack $ IS.toList s

count :: BoolGrid v -> Int
count (BoolGrid s) = IS.size s

fromList :: Vec v => [(v, Bool)] -> BoolGrid v
fromList = BoolGrid . IS.fromList . map (pack . fst) . filter snd

from2dSlice :: Vec v => [[Bool]] -> BoolGrid v
from2dSlice rows =
    let tagRow y = zipWith (\x v -> (from2d (x, y), v)) [0..]
        tagged = concat $ zipWith tagRow [0..] rows
     in fromList tagged
