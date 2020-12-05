grid `at` (x, y) = case drop y grid of
    []    -> Nothing
    row:_ -> Just $ row !! x

countTreesInLine grid (dx, dy) = countFrom (0, 0)
  where
    countFrom (x, y) =
        let rest = countFrom (x+dx, y+dy)
         in case grid `at` (x, y) of
                Nothing  -> 0
                Just '.' -> rest
                Just '#' -> 1 + rest

main = do
    grid <- map cycle . lines <$> getContents
    print $ countTreesInLine grid (3, 1)
    let slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
    print $ product $ map (countTreesInLine grid) slopes
