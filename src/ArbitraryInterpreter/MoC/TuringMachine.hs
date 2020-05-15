module ArbitraryInterpreter.MoC.TuringMachine
( turingMachine
) where

import ArbitraryInterpreter.Defs
import Data.Maybe (fromJust, isNothing)
import Text.Read (readMaybe)

-- checks the given arguments before passing them to MoC builder, precisely:
-- number of arguments is 1
-- arg0 is a string description of a valid TM alphabet (as checked by other function)
turingMachine :: [String] -> MoC
turingMachine args
    | length args /= 1 = error $ err ++ "Incorrect number of arguments"
    | badAlphabet = error $ err ++ "Expected valid TM alphabet, got: " ++ args !! 0
    | otherwise = buildTM (fromJust arg0)
    where
        arg0 = readMaybe (args !! 0) :: Maybe String
        badAlphabet = (isTMAlphabet <$> arg0) /= Just True
        err = "Error parsing arguments for turing machine: "


-- TODO tests in parsemoc, parsecollection, run
buildTM :: String -> MoC
buildTM alphabet = MoC (isValidTMState alphabet) ops preds
    where
        -- shorthands
        apply = applyTMOperation
        check = checkTMPredicate
        blank = head alphabet -- blank symbol

        ops opname = case opname of
            "NOP" -> Just id
            "L" -> Just $ apply (moveLeft blank)
            "R" -> Just $ apply (moveRight blank)
            ('W':s:[]) -> if s `elem` alphabet
                then Just $ apply (write s)
                else Nothing
            ('W':s:d:[]) -> if s `elem` alphabet && d `elem` "LNR"
                then Just $ apply (move d blank . write s)
                else Nothing
            _ -> Nothing

        preds predname = case predname of
            ('=':s:[]) -> if s `elem` alphabet
                then Just $ check (isSymbol s)
                else Nothing


isTMAlphabet :: String -> Bool
isTMAlphabet "" = False
isTMAlphabet alphabet = all (\s -> s `elem` vs) alphabet
    where
        vs = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "!%&()*+,-.;<>?@[]^_{|}~"


isValidTMState :: String -> MachineState -> Bool
isValidTMState alphabet ms
    | isNothing ms' = False -- ms needs to be readable
    | not $ isTMAlphabet alphabet = False -- alphabet needs to be valid
    | pos < 0 || pos >= length tape = False -- head needs to be in bounds
    | any (\s -> not $ s `elem` alphabet) tape =  False -- all symbols need to be valid
    | otherwise = True
    where
        ms'  = readMaybe ms :: Maybe (String, Int)
        (tape, pos) = fromJust ms'


-- reads machine state, applies operation and then returns string form of
-- machine state again
applyTMOperation :: ((String, Int) -> (String, Int)) -> MachineState -> MachineState
applyTMOperation op ms = show $ op (read ms :: (String, Int))


-- reads machine state and checks predicate
checkTMPredicate :: ((String, Int) -> Bool) -> MachineState -> Bool
checkTMPredicate p ms = p (read ms :: (String, Int))


isSymbol :: Char -> (String, Int) -> Bool
isSymbol s (tape, pos) = tape !! pos == s


moveLeft :: Char -> (String, Int) -> (String, Int)
moveLeft bl (tape, pos)
    | pos <= 0 = (bl:tape, 0) -- position stays at 0
    | otherwise = (tape, pos - 1)


moveRight :: Char -> (String, Int) -> (String, Int)
moveRight bl (tape, pos)
    | pos >= rbound = (tape ++ [bl], rbound + 1) -- position is new right bound
    | otherwise = (tape, pos + 1)
    where
        rbound = length tape - 1


move :: Char -> Char -> (String, Int) -> (String, Int)
move direction bl mstate = case direction of
    'L' -> moveLeft bl mstate
    'R' -> moveRight bl mstate
    _   -> mstate -- case 'N' or any other symbol


write :: Char -> (String, Int) -> (String, Int)
write s (tape, pos) = (left ++ (s:right), pos)
    where
        (left, (_:right)) = splitAt pos tape
