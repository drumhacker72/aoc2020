module Day8(Day8) where

import Data.Char (isDigit)
import Data.IntSet (empty, insert, member)
import Data.Sequence (Seq, fromList, index, update)
import Text.ParserCombinators.ReadP (ReadP, (+++))
import qualified Text.ParserCombinators.ReadP as P
import Day

data Op = Acc | Jmp | Nop
data Inst = Inst Op Int
type Program = Seq Inst

sign = (P.char '+' >> return 1) +++ (P.char '-' >> return (-1))
signedNum = do
    s <- sign
    n <- read <$> P.munch1 isDigit
    return $ s * n
inst = do
    op <- (P.string "acc" >> return Acc) +++ (P.string "jmp" >> return Jmp) +++ (P.string "nop" >> return Nop)
    P.char ' '
    Inst op <$> signedNum
readInst = (\[(r, "")] -> r) . P.readP_to_S inst

run :: Program -> (Bool, Int)
run insts = run' 0 0 empty
  where
    run' ip acc seen
        | ip `member` seen   = (False, acc)
        | ip >= length insts = (True, acc)
        | otherwise          =
            let seen' = insert ip seen
             in case insts `index` ip of
                Inst Acc n -> run' (ip+1) (acc+n) seen'
                Inst Jmp n -> run' (ip+n) acc     seen'
                Inst Nop _ -> run' (ip+1) acc     seen'

patch :: Program -> Int
patch insts = patch' 0
  where
    patch' ip = case insts `index` ip of
        Inst Jmp n ->
            let insts' = update ip (Inst Nop n) insts
                (ok, acc) = run insts'
             in if ok then acc else patch' (ip+1)
        _ -> patch' (ip+1)

newtype Day8 = D8 { runD8 :: Program }
instance Day Day8 where
    readDay _ = D8 . fromList . map readInst . lines
    part1 = show . snd . run . runD8
    part2 = show . patch . runD8
