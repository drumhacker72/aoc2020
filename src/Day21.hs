module Day21 (day21) where

import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Char (isLower)
import Data.List (intercalate, nub)
import Data.Map.Strict (Map)
import Data.Sequence (Seq((:<|)), (><))
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Text.ParserCombinators.ReadP as P
import Day (statefulDay)

data Food = Food { ingredients :: [Ingredient], allergens :: [Allergen] }
newtype Ingredient = I { runI :: String }
    deriving Eq
newtype Allergen = A { runA :: String }
    deriving (Eq, Ord)

ingredient = I <$> P.munch1 isLower
allergen = A <$> P.munch1 isLower
food = do
    ingredients <- P.sepBy1 ingredient (P.char ' ')
    P.string " (contains "
    allergens <- P.sepBy1 allergen (P.string ", ")
    P.char ')'
    return $ Food ingredients allergens
readFood s = case P.readP_to_S food s of [(f, "")] -> f
readFoods = map readFood . lines

mappings
    :: [Allergen] -> Seq Ingredient
    -> ReaderT [Food] [] (Map Allergen Ingredient)
mappings [] _ = return M.empty
mappings (algn:restAlgns) ingrs = do
    i <- lift [0 .. length ingrs - 1]
    let (leftIngrs, ingr :<| rightIngrs) = Seq.splitAt i ingrs
        restIngrs = leftIngrs >< rightIngrs
    guard =<< validPair algn ingr
    M.insert algn ingr <$> mappings restAlgns restIngrs

validPair :: Monad m => Allergen -> Ingredient -> ReaderT [Food] m Bool
validPair algn ingr = all ok <$> ask
  where
    ok (Food ingrs algns) = algn `notElem` algns || ingr `elem` ingrs

validMappings :: [Food] -> [Map Allergen Ingredient]
validMappings foods =
    let algns = nub $ concatMap allergens foods
        ingrs = Seq.fromList $ nub $ concatMap ingredients foods
     in runReaderT (mappings algns ingrs) foods

day21 = statefulDay readFoods part1 part2
  where
    part1 foods =
        let [m] = validMappings foods
            ingrs = concatMap ingredients foods
         in (m, show $ length $ filter (`notElem` M.elems m) ingrs)
    part2 _ m = intercalate "," $ map (runI . snd) $ M.toAscList m
