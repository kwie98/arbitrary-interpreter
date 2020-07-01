module ArbitraryInterpreter.MoC.TuringMachine
( turingMachine
) where

import ArbitraryInterpreter.Defs
import Data.Maybe (fromJust, isNothing)
import Text.Read (readMaybe)

-- inner machine state format, a MachineState is just the "shown" version of a TMState
type TMState = (String, Int)

-- checks the given arguments before passing them to MoC builder, precisely:
-- number of arguments is 1
-- arg0 is a string description of a valid TM alphabet (as checked by other function)
turingMachine :: [String] -> MoC
turingMachine args
    | length args /= 1 =
        error $ err ++ "Incorrect number of arguments"
    | badAlphabet =
        error $ err ++ "Expected valid TMMOC alphabet, got: " ++ args !! 0 ++
        " (Alphabet can only include these symbols: " ++ validSymbols ++ ")"
    | otherwise =
        buildTM (fromJust arg0)
    where
        arg0 = readMaybe (args !! 0) :: Maybe String
        badAlphabet = (isTMAlphabet <$> arg0) /= Just True
        err = "Error parsing arguments for TMMOC: "


buildTM :: String -> MoC
buildTM alphabet = MoC (isValidTMState alphabet) ops preds
    where
        -- shorthands
        apply = applyTMOperation
        check = checkTMPredicate
        blank = head alphabet -- blank symbol

        ops opname = case opname of
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
isTMAlphabet alphabet = all (\s -> s `elem` validSymbols) alphabet


isValidTMState :: String -> MachineState -> Maybe String
isValidTMState alphabet ms
    | isNothing ms' = -- ms needs to be readable
        Just "Machine state needs to have format (String, Int)"
    | pos < 0 || pos >= length tape = -- head needs to be in bounds
        Just "Tape head cannot be out of bounds"
    | any (\s -> not $ s `elem` alphabet) tape = -- all symbols need to be valid
        Just "Tape can only consist of symbols from the alphabet"
    | otherwise = Nothing
    where
        ms'  = readMaybe ms :: Maybe TMState
        (tape, pos) = fromJust ms'


-- reads machine state, applies operation and then returns string form of
-- machine state again
applyTMOperation :: (TMState -> TMState) -> MachineState -> MachineState
applyTMOperation op ms = show $ op (read ms :: TMState)


-- reads machine state and checks predicate
checkTMPredicate :: (TMState -> Bool) -> MachineState -> Bool
checkTMPredicate p ms = p (read ms :: TMState)


isSymbol :: Char -> TMState -> Bool
isSymbol s (tape, pos) = tape !! pos == s


moveLeft :: Char -> TMState -> TMState
moveLeft bl (tape, pos)
    | pos <= 0 = (bl:tape, 0) -- position stays at 0
    | otherwise = (tape, pos - 1)


moveRight :: Char -> TMState -> TMState
moveRight bl (tape, pos)
    | pos >= rbound = (tape ++ [bl], rbound + 1) -- position is new right bound
    | otherwise = (tape, pos + 1)
    where
        rbound = length tape - 1


move :: Char -> Char -> TMState -> TMState
move direction bl mstate = case direction of
    'L' -> moveLeft bl mstate
    'R' -> moveRight bl mstate
    _   -> mstate -- case 'N' or any other symbol


write :: Char -> TMState -> TMState
write s (tape, pos) = (left ++ (s:right), pos)
    where
        (left, (_:right)) = splitAt pos tape
