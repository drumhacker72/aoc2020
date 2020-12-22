module Day14 (day14) where

import Control.Monad.Trans.State (evalState, get, put)
import Data.Bits (clearBit, setBit)
import Data.Char (isDigit)
import Data.Foldable (foldlM)
import Data.IntMap (elems, empty, insert, singleton, union, unions)
import Text.ParserCombinators.ReadP ((+++))
import qualified Text.ParserCombinators.ReadP as P
import Day (statelessDay)

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

inst = mask +++ write

readInst s = case P.readP_to_S inst s of [(is, "")] -> is
readProgram = map readInst . lines

readMask1 = foldl (.) id . zipWith doBit [0..] . reverse
  where
    doBit i '0' = (`clearBit` i)
    doBit i '1' = (`setBit` i)
    doBit _ 'X' = id

readMask2 = foldl combine [id] . zipWith doBit [0..] . reverse
  where
    doBit _ '0' = [id]
    doBit i '1' = [(`setBit` i)]
    doBit i 'X' = [(`clearBit` i), (`setBit` i)]
    combine fs gs = [ f . g | f <- fs, g <- gs ]

run1 mem (NewMask s) = put (readMask1 s) >> return mem
run1 mem (Write addr value) = do
    m <- get
    return $ insert addr (m value) mem

run2 mem (NewMask s) = put (readMask2 s) >> return mem
run2 mem (Write addr value) = do
    m <- get
    let addrs = map ($ addr) m
        writes = unions $ map (`singleton` value) addrs
    return $ writes `union` mem

day14 = statelessDay readProgram part1 part2
  where
    part1 = show . sum . elems . (`evalState` undefined) . foldlM run1 empty
    part2 = show . sum . elems . (`evalState` undefined) . foldlM run2 empty
