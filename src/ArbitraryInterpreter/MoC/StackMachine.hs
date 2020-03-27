module ArbitraryInterpreter.MoC.StackMachine
( stackMachine
, isSMAlphabet
) where

import ArbitraryInterpreter.Defs
import Text.Read
import Data.Maybe

-- count and check arguments before passing to builder
stackMachine :: [String] -> MoC
stackMachine args
    | length args /= 3        = error $ err ++ "Incorrect number of arguments"
    | isNothing marg1         = error $ err ++ "Expected number of registers, got: " ++ args !! 1
    | arg1 < 1                = error $ err ++ "Number of registers needs to be greater or equal to 1"
    | isNothing marg2         = error $ err ++ "Expected alphabet, got: " ++ args !! 2 ++ " (express alphabet as symbols in String form with enclosing quotation marks)"
    | not $ isSMAlphabet arg2 = error $ err ++ "Alphabet needs to be non-empty and can only consist of alphanumerical symbols"
    | otherwise               = buildSM arg1 arg2
    where
        marg1 = readMaybe (args !! 1) :: Maybe Int
        arg1  = fromJust marg1
        marg2 = readMaybe (args !! 2) :: Maybe String
        arg2  = fromJust marg2
        err   = "Error parsing arguments for stack machine: "


-- build a stack machine with a given number of registers over the given
-- alphabet (consisting only of alphanumerical symbols). Each stack is a string
-- with the top element being on the left side
buildSM :: Int -> String -> MoC
buildSM numRegs alphabet = MoC (isValidSMState numRegs alphabet) ops preds
    where
        ops opCode
            | valid && isPop  = Just (\regs -> (popSM numRegs r regs))
            | valid           = Just (\regs -> (pushSM numRegs r regs s))
            | opCode == "NOP" = Just id
            | otherwise       = Nothing
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
    (map (\(reg, symbol) -> ("R" ++ (show reg) ++ "+" ++ [symbol])) [(a, b) | a <- [1..numRegs], b <- alphabet]) ++
    (map (\reg -> ("R" ++ (show reg) ++ "-")) [1..numRegs])


-- Rr=s, Rr=_
validSMPredicates :: Int -> String -> [String]
validSMPredicates numRegs alphabet =
    (map (\(reg, symbol) -> ("R" ++ (show reg) ++ "=" ++ [symbol])) [(a, b) | a <- [1..numRegs], b <- alphabet]) ++
    (map (\reg -> ("R" ++ (show reg) ++ "=_")) [1..numRegs])


isValidSMState :: Int -> String -> MachineState -> Bool
isValidSMState r alphabet regs
    | isNothing regs'             = False
    | not $ isSMAlphabet alphabet = False
    | length regs'' == r          = and (map (all (\s -> s `elem` alphabet)) regs'') -- check that all registers only have valid symbols
    | otherwise                   = False
        where
            regs'  = readMaybe regs :: Maybe [String]
            regs'' = fromJust regs'


isSMAlphabet :: String -> Bool
isSMAlphabet alphabet =
    (not . null $ alphabet) &&
    all (\symbol -> symbol `elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']) alphabet


isEmptySM :: Int -> Int -> MachineState -> Bool
isEmptySM numRegs r regs
    | isInvalidRegister numRegs r = error (oobErrMsg numRegs r)
    | otherwise                   = regs' !! (r - 1) == ""
        where regs' = read regs :: [String]


isSymbolSM :: Int -> Int -> Char -> MachineState -> Bool
isSymbolSM numRegs r s regs
    | isInvalidRegister numRegs r = error (oobErrMsg numRegs r)
    | isEmptySM numRegs r regs    = False
    | otherwise                   = last (regs' !! (r - 1)) == s
        where regs' = read regs :: [String]


popSM :: Int -> Int -> MachineState -> MachineState
popSM numRegs r regs
    | isInvalidRegister numRegs r = error (oobErrMsg numRegs r)
    | isEmptySM numRegs r regs    = regs -- don't change state when the register is already empty
    | numRegs == r                = show $ fst split ++ [init (head (snd split))] -- when popping from last register
    | otherwise                   = show $ fst split ++ [init (head (snd split))] ++ tail (snd split)
        where
            split = splitAt (r - 1) regs'
            regs' = read regs :: [String]


pushSM :: Int -> Int -> MachineState -> Char -> MachineState
pushSM numRegs r regs val
    | isInvalidRegister numRegs r = error (oobErrMsg numRegs r)
    | numRegs == r                = show $ fst split ++ [(head (snd split)) ++ [val]] -- when modifying last register
    | otherwise                   = show $ fst split ++ [(head (snd split)) ++ [val]] ++ tail (snd split)
        where
            split = splitAt (r - 1) regs'
            regs' = read regs :: [String]


isInvalidRegister :: Int -> Int -> Bool
isInvalidRegister numRegs r
    | r > numRegs = True
    | r < 1       = True
    | otherwise   = False

oobErrMsg k i = "tried to access register " ++ (show i) ++ " of stack machine with " ++ (show k) ++ " registers"
