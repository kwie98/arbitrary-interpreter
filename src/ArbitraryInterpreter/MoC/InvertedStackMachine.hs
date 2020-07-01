module ArbitraryInterpreter.MoC.InvertedStackMachine (
invertedStackMachine
) where

import ArbitraryInterpreter.Defs
import ArbitraryInterpreter.MoC.StackMachine (isSMAlphabet)
import Text.Read
import Data.Maybe

-- count and check arguments before passing to builder
invertedStackMachine :: [String] -> MoC
invertedStackMachine args
    | length args /= 2 =
        error $ err ++ "Incorrect number of arguments"
    | isNothing marg0 =
        error $ err ++ "Expected number of registers, got: " ++ args !! 0
    | arg0 < 1 =
        error $ err ++ "Number of registers needs to be greater or equal to 1"
    | isNothing marg1 =
        error $ err ++ "Expected valid ISMMOC alphabet, got: " ++ args !! 1 ++
        " (Alphabet can only include these symbols: " ++ validSMSymbols ++ ")"
    | not $ isISMAlphabet arg1 =
        error $ err ++ "Alphabet needs to be non-empty and can only include " ++
        "these symbols: " ++ validSMSymbols
    | otherwise =
        buildISM arg0 arg1
    where
        marg0 = readMaybe (args !! 0) :: Maybe Int
        arg0  = fromJust marg0
        marg1 = readMaybe (args !! 1) :: Maybe String
        arg1  = fromJust marg1
        err   = "Error parsing arguments for ISMMOC: "


-- build a stack machine with a given number of registers over the given
-- alphabet (consisting only of alphanumerical symbols). Each stack is a string
-- with the top element being on the left side
buildISM :: Int -> String -> MoC
buildISM numRegs alphabet = MoC (isValidISMState numRegs alphabet) ops preds
    where
        ops opname
            | valid && isPop  = Just (\regs -> (popISM numRegs r regs))
            | valid           = Just (\regs -> (pushISM numRegs r regs s))
            | otherwise       = Nothing
                where
                    valid = elem opname $ validISMOperations numRegs alphabet
                    isPop = last opname == '-' -- whether operation code describes a pop operation on a register
                    r     = if last opname == '-' -- register
                                then (read . init $ tail opname) :: Int
                                else (read . init . init $ tail opname) :: Int
                    s     = last opname -- symbol
        preds predname
            | valid && isEmptyCheck = Just $ isEmptyISM numRegs r
            | valid                 = Just $ isSymbolISM numRegs r s
            | otherwise             = Nothing
                where
                    valid        = elem predname $ validISMPredicates numRegs alphabet
                    isEmptyCheck = last predname == '_' -- whether predicate code describes an empty check operation on a register
                    r            = (read . init . init $ tail predname) :: Int -- register
                    s            = last predname -- symbol


-- return a list of all possible Operations for a stack machine with numRegs registers and the given alphabet
-- Rr+s, Rr-
validISMOperations :: Int -> String -> [String]
validISMOperations numRegs alphabet =
    (map (\(reg, symbol) -> ("R" ++ (show reg) ++ "+" ++ [symbol])) [(a, b) | a <- [1..numRegs], b <- alphabet]) ++
    (map (\reg -> ("R" ++ (show reg) ++ "-")) [1..numRegs])


-- Rr=s, Rr=_
validISMPredicates :: Int -> String -> [String]
validISMPredicates numRegs alphabet =
    (map (\(reg, symbol) -> ("R" ++ (show reg) ++ "=" ++ [symbol])) [(a, b) | a <- [1..numRegs], b <- alphabet]) ++
    (map (\reg -> ("R" ++ (show reg) ++ "=_")) [1..numRegs])


isValidISMState :: Int -> String -> MachineState -> Maybe String
isValidISMState r alphabet regs
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


isISMAlphabet :: String -> Bool
isISMAlphabet = isSMAlphabet


isEmptyISM :: Int -> Int -> MachineState -> Bool
isEmptyISM numRegs r regs
    | isInvalidRegister numRegs r = error (oobErrMsg numRegs r)
    | otherwise                   = regs' !! (r - 1) == ""
        where regs' = read regs :: [String]


isSymbolISM :: Int -> Int -> Char -> MachineState -> Bool
isSymbolISM numRegs r s regs
    | isInvalidRegister numRegs r = error (oobErrMsg numRegs r)
    | isEmptyISM numRegs r regs   = False
    | otherwise                   = (head (regs' !! (r - 1)) == s)
        where regs' = read regs :: [String]


popISM :: Int -> Int -> MachineState -> MachineState
popISM numRegs r regs
    | isInvalidRegister numRegs r = error (oobErrMsg numRegs r)
    | isEmptyISM numRegs r regs   = regs -- don't change state when the register is already empty
    | numRegs == r                = show $ fst split ++ [tail (head (snd split))] -- when popping from last register
    | otherwise                   = show $ fst split ++ [tail (head (snd split))] ++ tail (snd split)
        where
            split = splitAt (r - 1) regs'
            regs' = read regs :: [String]


pushISM :: Int -> Int -> MachineState -> Char -> MachineState
pushISM numRegs r regs val
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
