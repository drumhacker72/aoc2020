find2 entries = head
    [ e1 * e2 | e1 <- entries, e2 <- entries, e1 + e2 == 2020 ]

find3 entries = head
    [ e1 * e2 * e3 | e1 <- entries, e2 <- entries, e3 <- entries, e1 + e2 + e3 == 2020 ]

main = do
    entries <- map (read :: String -> Int) . lines <$> getContents
    print $ find2 entries
    print $ find3 entries
