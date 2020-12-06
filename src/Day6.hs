import qualified Data.Set as S

group ls =
    let (p, rest) = break null ls
     in p : case rest of
         []    -> []
         _:ls' -> group ls'

main = do
    groups <- map (map S.fromList) . group . lines <$> getContents
    print $ sum $ map (S.size . S.unions) groups
    print $ sum $ map (S.size . foldl1 S.intersection) groups
