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
    | length args /= 2 =
        error $ err ++ "Incorrect number of arguments"
    | isNothing marg0 =
        error $ err ++ "Expected number of registers, got: " ++ args !! 0
    | arg0 < 1 =
        error $ err ++ "Number of registers needs to be greater or equal to 1"
    | isNothing marg1 =
        error $ err ++ "Expected valid SMMOC alphabet, got: " ++ args !! 1
    | not $ isSMAlphabet arg1 =
        error $ err ++ "Alphabet needs to be non-empty and can only include " ++
        "these symbols: " ++ validSMSymbols
    | otherwise =
        buildSM arg0 arg1
    where
        marg0 = readMaybe (args !! 0) :: Maybe Int
        arg0  = fromJust marg0
        marg1 = readMaybe (args !! 1) :: Maybe String
        arg1  = fromJust marg1
        err   = "Error parsing arguments for SMMOC: "


-- build a stack machine with a given number of registers over the given
-- alphabet (consisting only of alphanumerical symbols). Each stack is a string
-- with the top element being on the left side
buildSM :: Int -> String -> MoC
buildSM numRegs alphabet = MoC (isValidSMState numRegs alphabet) ops preds
    where
        ops opname
            | valid && isPop  = Just (\regs -> (popSM numRegs r regs))
            | valid           = Just (\regs -> (pushSM numRegs r regs s))
            | otherwise       = Nothing
                where
                    valid = elem opname $ validSMOperations numRegs alphabet
                    isPop = last opname == '-' -- whether operation code describes a pop operation on a register
                    r     = if last opname == '-' -- register
                                then (read . init $ tail opname) :: Int
                                else (read . init . init $ tail opname) :: Int
                    s     = last opname -- symbol
        preds predname
            | valid && isEmptyCheck = Just $ isEmptySM numRegs r
            | valid                 = Just $ isSymbolSM numRegs r s
            | otherwise             = Nothing
                where
                    valid        = elem predname $ validSMPredicates numRegs alphabet
                    isEmptyCheck = last predname == '_' -- whether predicate code describes an empty check operation on a register
                    r            = (read . init . init $ tail predname) :: Int -- register
                    s            = last predname -- symbol


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


isValidSMState :: Int -> String -> MachineState -> Maybe String
isValidSMState r alphabet regs
    | isNothing regs' =
        Just "Machine state needs to have format [String]"
    | length regs'' /= r =
        Just $ "Machine state needs to be [String] of length " ++ show r
    | any (invalidSymbols) regs'' =
        Just "Registers can only hold symbols from the alphabet"
    | otherwise = Nothing
        where
            regs'  = readMaybe regs :: Maybe [String]
            regs'' = fromJust regs'
            invalidSymbols = \reg -> any (\s -> s `notElem` alphabet) reg


isSMAlphabet :: String -> Bool
isSMAlphabet alphabet =
    (not . null $ alphabet) &&
    all (\symbol -> symbol `elem` validSMSymbols) alphabet


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
