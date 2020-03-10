module MOC.CounterMachine where

import Text.Read
import Data.Maybe
import MOC.MOC

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ x = x

counterMachine :: Int -> MoC
counterMachine numRegs = MoC (validCMState numRegs) ops preds
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
    ((map (\reg -> ("R"++(show reg)++"+1")) [1..numRegs]) ++ (map (\reg -> ("R"++(show reg)++"-1")) [1..numRegs]))


-- Rr=0
validCMPredicates :: Int -> [String]
validCMPredicates numRegs =
    ((map (\reg -> ("R"++(show reg)++"=0")) [1..numRegs]))


-- check if given machine state fits given number of registers of the machine
validCMState :: Int -> String -> Bool
validCMState r regs
    | isNothing regs'    = False
    | length regs'' == r = True
    | otherwise          = False
    where
        regs'  = readMaybe regs :: Maybe [Int]
        regs'' = fromJust regs'


getCMRegisterValue :: Int -> Int -> String -> Int
getCMRegisterValue numRegs r regs
    | isInvalidRegister numRegs r = error (oobErrMsg numRegs r)
    | otherwise                   = regs' !! (r - 1)
    where regs' = read regs :: [Int]


setCMRegisterValue :: Int -> Int -> String -> Int -> String
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

oobErrMsg k i = ("tried to access register "++(show i)++" of counter machine with "++(show k)++" registers")

showCMState k ms = (show (map (\i -> (getCMRegisterValue k i ms)) [1..k]))

--EXAMPLES
cm4 = (counterMachine 4)
show1 = (showCMState 4)

inc :: Int -> (String -> String)
inc i = (fromJust (ops cm4 ("R"++(show i)++"+1")))
dec :: Int -> (String -> String)
dec i = (fromJust (ops cm4 ("R"++(show i)++"-1")))
is0 :: Int -> (String -> Bool)
is0 i = (fromJust (preds cm4 ("R"++(show i)++"=0")))


z4 = show [1, 2, 3, 0]
z5 = (inc 1 z4)
z6 = (dec 1 z5)
z7 = (dec 4 z6)
