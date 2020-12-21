module Day4(Day4) where

import Control.Monad (guard)
import Data.Char (isDigit, isLower)
import Data.Maybe (isJust)
import qualified Text.ParserCombinators.ReadP as P
import Day

type Passport = [(String, String)]
field = do
    key <- P.munch1 isLower
    P.char ':'
    value <- P.munch1 (\c -> isLower c || isDigit c || c == '#')
    return (key, value)
passportLine = P.sepBy1 field (P.char ' ')
passport = concat <$> P.sepBy1 passportLine (P.char '\n')
passportList = P.sepBy1 passport (P.string "\n\n")
readPassports s = case P.readP_to_S (passportList <* P.skipSpaces <* P.eof) s of
    [(ps, "")] -> ps

hasRequired p = isJust hasRequiredM
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

valid p = isJust validM
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

newtype Day4 = D4 { runD4 :: [Passport] }
instance Day Day4 where
    readDay _ = D4 . readPassports
    part1 = show . length . filter id . map hasRequired . runD4
    part2 = show . length . filter id . map valid . runD4
