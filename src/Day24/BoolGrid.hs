module Day24.BoolGrid
    ( BoolGrid
    , mkGrid
    , get
    , set
    , elems
    , count
    , fromList
    ) where

import Data.Set (Set)
import qualified Data.Set as S

newtype BoolGrid i = BoolGrid (Set (i, i))

mkGrid :: BoolGrid i
mkGrid = BoolGrid S.empty

get :: Ord i => (i, i) -> BoolGrid i -> Bool
get p (BoolGrid s) = p `S.member` s

set :: Ord i => (i, i) -> Bool -> BoolGrid i -> BoolGrid i
set p False (BoolGrid s) = BoolGrid $ S.delete p s
set p True  (BoolGrid s) = BoolGrid $ S.insert p s

elems :: BoolGrid i -> Set (i, i)
elems (BoolGrid s) = s

count :: BoolGrid v -> Int
count (BoolGrid s) = S.size s

fromList :: Ord i => [((i, i), Bool)] -> BoolGrid i
fromList = BoolGrid . S.fromList . map fst . filter snd
