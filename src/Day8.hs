import Data.Char (isDigit)
import Data.IntSet (empty, insert, member)
import Data.Sequence (Seq, fromList, index, update)
import Text.ParserCombinators.ReadP (ReadP, readP_to_S, (+++))

import qualified Text.ParserCombinators.ReadP as P

data Op = Acc | Jmp | Nop
data Inst = Inst Op Int
type Program = Seq Inst

sign :: ReadP Int
sign = (P.char '+' >> return 1) +++ (P.char '-' >> return (-1))

signedNum :: ReadP Int
signedNum = do
    s <- sign
    n <- read <$> P.munch1 isDigit
    return $ s * n

inst :: ReadP Inst
inst = do
    op <- (P.string "acc" >> return Acc) +++ (P.string "jmp" >> return Jmp) +++ (P.string "nop" >> return Nop)
    P.char ' '
    Inst op <$> signedNum

readInst :: String -> Inst
readInst = (\[(r, "")] -> r) . readP_to_S inst

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

main = do
    insts <- fromList . map readInst . lines <$> getContents
    print $ snd $ run insts
    print $ patch insts
