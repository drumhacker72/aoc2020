import Data.Char (isDigit)
import Text.ParserCombinators.ReadP (ReadP, readP_to_S, (+++))
import Data.Vector (Vector, fromList, (!?), (!), (//))
import Data.IntSet (IntSet, member, insert, empty)
import Control.Monad.Trans.State (State, get, modify, runState)

import Debug.Trace
import qualified Text.ParserCombinators.ReadP as P

signedNum :: ReadP Int
signedNum = do
    sign <- (P.char '+' >> return 1) +++ (P.char '-' >> return (-1))
    n <- read <$> P.munch1 isDigit
    return $ sign * n

data Op
    = Acc
    | Jmp
    | Nop
    deriving Show

data Inst = Inst Op Int
    deriving Show

inst :: ReadP Inst
inst = do
    op <- (P.string "acc" >> return Acc) +++ (P.string "jmp" >> return Jmp) +++ (P.string "nop" >> return Nop)
    P.char ' '
    arg <- signedNum
    return $ Inst op arg

readInst :: String -> Inst
readInst = (\[(r, "")] -> r) . readP_to_S inst

step :: Int -> Int -> Vector Inst -> State IntSet (Bool, Int)
step acc i insts = do
    seen <- get
    if i `member` seen
    then return (False, acc)
    else do
        modify $ insert i
        case insts !? i of
            Nothing -> return (True, acc)
            Just (Inst Acc n) -> step (acc+n) (i+1) insts
            Just (Inst Jmp n) -> step acc (i+n) insts
            Just (Inst Nop n) -> step acc (i+1) insts

derp :: Int -> Vector Inst -> Int
derp i insts =
    case insts ! i of
        Inst Jmp n ->
            let insts' = insts // [(i, Inst Nop n)]
                ((done, acc), _) = runState (step 0 0 insts') empty
             in if done then acc else derp (i+1) insts
        _ -> derp (i+1) insts

main = do
    insts <- fromList . map readInst . lines <$> getContents
    print $ snd $ fst $ runState (step 0 0 insts) empty
    print $ derp 0 insts
