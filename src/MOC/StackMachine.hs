module MoC.StackMachine (
stackMachine
) where

import Text.Read
import Data.Maybe
import MoC.MoC

-- build a stack machine with a given number of registers over the given
-- alphabet (consisting only of alphanumerical symbols). Each stack is a string with the top
-- top element being on the left side
stackMachine :: Int -> String -> MoC
stackMachine numRegs alphabet = MoC (validSMState numRegs alphabet) ops preds
    where
        ops opCode
            | valid && isPop = Just (\regs -> (popSM numRegs r regs))
            | valid          = Just (\regs -> (pushSM numRegs r regs s))
            | otherwise      = Nothing
                where
                    valid = elem opCode $ validSMOperations numRegs alphabet
                    isPop = last opCode == '-' -- whether operation code describes a pop operation on a register
                    r     = if last opCode == '-' -- register
                                then (read . init $ tail opCode) :: Int
                                else (read . init . init $ tail opCode) :: Int
                    s     = last opCode -- symbol
        preds predCode
            | valid && isEmptyCheck = Just $ isEmptySM numRegs r
            | valid                 = Just $ isSymbolSM numRegs r s
            | otherwise             = Nothing
                where
                    valid        = elem predCode $ validSMPredicates numRegs alphabet
                    isEmptyCheck = last predCode == '_' -- whether predicate code describes an empty check operation on a register
                    r            = (read . init . init $ tail predCode) :: Int -- register
                    s            = last predCode -- symbol


-- return a list of all possible Operations for a stack machine with numRegs registers and the given alphabet
-- Rr+s, Rr-
validSMOperations :: Int -> String -> [String]
validSMOperations numRegs alphabet =
    ((map (\(reg, symbol) -> ("R" ++ (show reg) ++ "+" ++ [symbol])) [(a, b) | a <- [1..numRegs], b <- alphabet]) ++
    (map (\reg -> ("R" ++ (show reg) ++ "-")) [1..numRegs]))

-- Rr=s, Rr=_
validSMPredicates :: Int -> String -> [String]
validSMPredicates numRegs alphabet =
    ((map (\(reg, symbol) -> ("R" ++ (show reg) ++ "=" ++ [symbol])) [(a, b) | a <- [1..numRegs], b <- alphabet]) ++
    (map (\reg -> ("R" ++ (show reg) ++ "=_")) [1..numRegs]))


validSMState :: Int -> String -> String -> Bool
validSMState r alphabet regs
    | isNothing regs'    = False
    | not validAlphabet  = False
    | length regs'' == r = and (map (all (flip elem alphabet)) regs'') -- check that all registers only have valid symbols
    | otherwise          = False
        where
            validAlphabet = all (flip elem $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']) alphabet
            regs'         = readMaybe regs :: Maybe [String]
            regs''        = fromJust regs'


isEmptySM :: Int -> Int -> String -> Bool
isEmptySM numRegs r regs
    | isInvalidRegister numRegs r = error (oobErrMsg numRegs r)
    | otherwise                   = regs' !! (r - 1) == ""
        where regs' = read regs :: [String]


isSymbolSM :: Int -> Int -> Char -> String -> Bool
isSymbolSM numRegs r s regs
    | isInvalidRegister numRegs r = error (oobErrMsg numRegs r)
    | isEmptySM numRegs r regs    = False
    | otherwise                   = (head (regs' !! (r - 1)) == s)
        where regs' = read regs :: [String]


popSM :: Int -> Int -> String -> String
popSM numRegs r regs
    | isInvalidRegister numRegs r = error (oobErrMsg numRegs r)
    | isEmptySM numRegs r regs    = regs -- don't change state when the register is already empty
    | numRegs == r                = show $ fst split ++ [tail (head (snd split))] -- when popping from last register
    | otherwise                   = show $ fst split ++ [tail (head (snd split))] ++ tail (snd split)
        where
            split = splitAt (r - 1) regs'
            regs' = read regs :: [String]


pushSM :: Int -> Int -> String -> Char -> String
pushSM numRegs r regs val
    | isInvalidRegister numRegs r = error (oobErrMsg numRegs r)
    | numRegs == r                = show $ fst split ++ [val : (head (snd split))] -- when modifying last register
    | otherwise                   = show $ fst split ++ [val : (head (snd split))] ++ tail (snd split)
        where
            split = splitAt (r - 1) regs'
            regs' = read regs :: [String]


isInvalidRegister :: Int -> Int -> Bool
isInvalidRegister numRegs r
    | r > numRegs = True
    | r < 1       = True
    | otherwise   = False


oobErrMsg k i = "tried to access register " ++ (show i) ++ " of stack machine with " ++ (show k) ++ " registers"

sm3 = (stackMachine 3 "ACEace012")

append :: Int -> Char -> (String -> String)
append r s = (fromJust (ops sm3 ("R" ++ (show r) ++ "+" ++ [s])))

remove :: Int -> (String -> String)
remove r = (fromJust (ops sm3 ("R" ++ (show r) ++ "-")))

isSymbol :: Int -> Char -> (String -> Bool)
isSymbol r s = (fromJust (preds sm3 ("R" ++ (show r) ++ "=" ++ [s])))

isEmpty :: Int -> (String -> Bool)
isEmpty r = (fromJust (preds sm3 ("R" ++ (show r) ++ "=_")))
