import Control.Monad (guard)
import Data.Char (isDigit)

group ls =
    let (p, rest) = break null ls
     in p : case rest of
         []    -> []
         _:ls' -> group ls'

parse = concatMap (map split . words)
  where
    split kv =
        let (k, _:v) = break (== ':') kv
         in (k, v)

hasRequired p = case hasRequiredM of
    Nothing -> False
    Just () -> True
  where
    hasRequiredM = do
        lookup "byr" p
        lookup "iyr" p
        lookup "eyr" p
        lookup "hgt" p
        lookup "hcl" p
        lookup "ecl" p
        lookup "pid" p
        return ()

valid p = case validM of
    Nothing -> False
    Just () -> True
  where
    validM = do
        byr <- lookup "byr" p
        guard $ all isDigit byr
        let byr' = read byr :: Int
        guard $ 1920 <= byr' && byr' <= 2002

        iyr <- lookup "iyr" p
        guard $ all isDigit iyr
        let iyr' = read iyr :: Int
        guard $ 2010 <= iyr' && iyr' <= 2020

        eyr <- lookup "eyr" p
        guard $ all isDigit eyr
        let eyr' = read eyr :: Int
        guard $ 2020 <= eyr' && eyr' <= 2030

        hgt <- lookup "hgt" p
        let (hgtn, hgtu) = splitAt (length hgt - 2) hgt
        guard $ all isDigit hgtn
        let hgtn' = read hgtn :: Int
        guard $ case hgtu of
            "cm" -> 150 <= hgtn' && hgtn' <= 193
            "in" -> 59 <= hgtn' && hgtn' <= 76
            _    -> False

        hcl <- lookup "hcl" p
        guard $ head hcl == '#' && all (\c -> isDigit c || c `elem` "abcdef") (tail hcl)

        ecl <- lookup "ecl" p
        guard $ ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

        pid <- lookup "pid" p
        guard $ length pid == 9 && all isDigit pid

        return ()

main = do
    ps <- map parse . group . lines <$> getContents
    print $ length $ filter id $ map hasRequired ps
    print $ length $ filter id $ map valid ps
