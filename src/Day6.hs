import Data.List.Split (splitWhen)
import qualified Data.Set as S

main = do
    groups <- map (map S.fromList) . splitWhen null . lines <$> getContents
    print $ sum $ map (S.size . S.unions) groups
    print $ sum $ map (S.size . foldl1 S.intersection) groups
