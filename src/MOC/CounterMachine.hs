module MoC.CounterMachine (
counterMachine
) where

import Text.Read
import Data.Maybe
import MoC.MoC

-- count and check arguments before passing to builder
counterMachine :: [String] -> MoC
counterMachine args
    | length args /= 2 = error $ err ++ "Incorrect number of arguments"
    | isNothing marg1  = error $ err ++ "Expected number of registers, got: " ++ args !! 1
    | arg1 < 1         = error $ err ++ "Number of registers needs to be greater or equal to 1"
    | otherwise        = buildCM arg1
    where
        marg1 = readMaybe (args !! 1) :: Maybe Int
        arg1  = fromJust marg1
        err   = "Error parsing arguments for counter machine: "


buildCM :: Int -> MoC
buildCM numRegs = MoC (isValidCMState numRegs) ops preds
    where
        getReg = (getCMRegisterValue numRegs)
        setReg = (setCMRegisterValue numRegs)
        ops opCode
            | valid && isAdd = Just (\ms -> (setReg r ms ((getReg r ms) + 1)))
            | valid          = Just (\ms -> (setReg r ms (max 0 ((getReg r ms) - 1))))
            | otherwise      = Nothing
            where
                -- check String representation of operation
                valid = elem opCode $ validCMOperations numRegs
                isAdd = elem '+' opCode
                r = (read . init . init $ tail opCode) :: Int
        preds predCode
            | valid     = Just (\ms -> ((getReg r ms) == 0))
            | otherwise = Nothing
            where
                valid = elem predCode $ validCMPredicates numRegs
                r = (read . init . init $ tail predCode) :: Int


-- return a list of all possible Operations for a counter machine with numRegs registers
-- Rr+1, Rr-1
validCMOperations :: Int -> [String]
validCMOperations numRegs =
    (map (\reg -> ("R"++(show reg)++"+1")) [1..numRegs]) ++
    (map (\reg -> ("R"++(show reg)++"-1")) [1..numRegs])


-- Rr=0
validCMPredicates :: Int -> [String]
validCMPredicates numRegs =
    ((map (\reg -> ("R"++(show reg)++"=0")) [1..numRegs]))


-- check if given machine state fits given number of registers of the machine
isValidCMState :: Int -> MachineState -> Bool
isValidCMState r regs
    | isNothing regs'    = False
    | length regs'' == r = True
    | otherwise          = False
    where
        regs'  = readMaybe regs :: Maybe [Int]
        regs'' = fromJust regs'


getCMRegisterValue :: Int -> Int -> MachineState -> Int
getCMRegisterValue numRegs r regs
    | isInvalidRegister numRegs r = error (oobErrMsg numRegs r)
    | otherwise                   = regs' !! (r - 1)
    where regs' = read regs :: [Int]


setCMRegisterValue :: Int -> Int -> MachineState -> Int -> MachineState
setCMRegisterValue numRegs r regs val
    | isInvalidRegister numRegs r = error (oobErrMsg numRegs r)
    | numRegs == r                = show $ fst split ++ [val]
    | otherwise                   = show $ fst split ++ val : tail (snd split)
        where
            split = splitAt (r - 1) regs'
            regs' = read regs :: [Int]


isInvalidRegister :: Int -> Int -> Bool
isInvalidRegister numRegs r
    | r > numRegs = True
    | r < 1       = True
    | otherwise   = False

oobErrMsg k i = "tried to access register " ++ (show i) ++ " of counter machine with " ++ (show k) ++ " registers"
