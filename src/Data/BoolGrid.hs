module Data.BoolGrid
    ( BoolGrid
    , mkGrid
    , get
    , set
    , elems
    , count
    , fromList
    , from2dSlice
    ) where

import Control.Vector (Vector(add, basis, from2d, invert, zero))
import Data.Set (Set)
import qualified Data.Set as S

newtype BoolGrid v = BoolGrid (Set v)

mkGrid :: Vector v => BoolGrid v
mkGrid = BoolGrid S.empty

get :: Vector v => v -> BoolGrid v -> Bool
get p (BoolGrid s) = p `S.member` s

set :: Vector v => v -> Bool -> BoolGrid v -> BoolGrid v
set p False (BoolGrid s) = BoolGrid $ S.delete p s
set p True  (BoolGrid s) = BoolGrid $ S.insert p s

elems :: BoolGrid v -> Set v
elems (BoolGrid s) = s

count :: BoolGrid v -> Int
count (BoolGrid s) = S.size s

fromList :: Vector v => [(v, Bool)] -> BoolGrid v
fromList = BoolGrid . S.fromList . map fst . filter snd

from2dSlice :: Vector v => [[Bool]] -> BoolGrid v
from2dSlice rows =
    let tagRow y = zipWith (\x v -> (from2d (x, y), v)) [0..]
        tagged = concat $ zipWith tagRow [0..] rows
     in fromList tagged
