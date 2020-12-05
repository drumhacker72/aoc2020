{-# LANGUAGE QuasiQuotes #-}

import Text.Scanf ((:+)((:+)), fmt, scanf)

isValid1 (min :+ max :+ letter :+ password :+ ()) = min <= count && count <= max
  where
    count = length $ filter (== letter) password

isValid2 (pos1 :+ pos2 :+ letter :+ password :+ ()) = at1 /= at2
  where
    at1 = password !! (pos1-1) == letter
    at2 = password !! (pos2-1) == letter

exactMatch [(e, "")] = e

main = do
    entries <- map (exactMatch . scanf [fmt|%d-%d %c: %s|]) . lines <$> getContents
    print $ length $ filter isValid1 entries
    print $ length $ filter isValid2 entries
