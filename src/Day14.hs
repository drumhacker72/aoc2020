module Day14(Day14) where

import Data.Bits (clearBit, setBit)
import Data.Char (isDigit)
import Data.IntMap (elems, empty, insert, singleton, union, unions)
import Text.ParserCombinators.ReadP ((+++))
import qualified Text.ParserCombinators.ReadP as P
import Day

data Inst = NewMask String | Write Int Int

mask = do
    P.string "mask = "
    NewMask <$> P.count 36 (P.char 'X' +++ P.char '0' +++ P.char '1')

write = do
    P.string "mem["
    addr <- read <$> P.munch1 isDigit
    P.string "] = "
    value <- read <$> P.munch1 isDigit
    return $ Write addr value

program = mask +++ write

readProgram :: String -> [Inst]
readProgram = map ((\[(is, "")] -> is) . P.readP_to_S program) . lines

readMask1 = foldl (.) id . zipWith doBit [0..] . reverse
  where
    doBit i '0' = (`clearBit` i)
    doBit i '1' = (`setBit` i)
    doBit _ 'X' = id

readMask2 = foldl combine [id] . zipWith doBit [0..] . reverse
  where
    doBit i '0' = [id]
    doBit i '1' = [(`setBit` i)]
    doBit i 'X' = [(`clearBit` i), (`setBit` i)]
    combine fs gs = [ f . g | f <- fs, g <- gs ]

run1 _ mem (NewMask m:rest) = run1 (readMask1 m) mem rest
run1 m mem (Write addr value:rest) = run1 m (insert addr (m value) mem) rest
run1 _ mem [] = mem

run2 _ mem (NewMask m:rest) = run2 (readMask2 m) mem rest
run2 m mem (Write addr value:rest) =
    let addrs = map ($ addr) m
        mem' = unions $ map (\addr -> singleton addr value) addrs
     in run2 m (union mem' mem) rest
run2 _ mem [] = mem

newtype Day14 = D14 { runD14 :: [Inst] }
instance Day Day14 where
    readDay _ = D14 . readProgram
    part1 = show . sum . elems . run1 undefined empty . runD14
    part2 = show . sum . elems . run2 undefined empty . runD14
