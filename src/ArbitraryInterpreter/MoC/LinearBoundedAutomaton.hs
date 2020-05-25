module ArbitraryInterpreter.MoC.LinearBoundedAutomaton
( linearBoundedAutomaton
) where

import ArbitraryInterpreter.Defs
import Data.Maybe (fromJust, isNothing)
import Text.Read (readMaybe)

-- inner machine state format, a MachineState is just the "shown" version of a LBAState
type LBAState = (String, Int, Bool)
-- TODO test stuff with crashing and combined operations!!!

-- checks the given arguments before passing them to MoC builder, precisely:
-- number of arguments is 1
-- arg0 is a string description of a valid TM alphabet (as checked by other function)
linearBoundedAutomaton :: [String] -> MoC
linearBoundedAutomaton args
    | length args /= 1 = error $ err ++ "Incorrect number of arguments"
    | badAlphabet = error $ err ++ "Expected valid LBA alphabet, got: " ++ args !! 0
    | otherwise = buildLBA (fromJust arg0)
    where
        arg0 = readMaybe (args !! 0) :: Maybe String
        badAlphabet = (isLBAAlphabet <$> arg0) /= Just True
        err = "Error parsing arguments for linear bounded automaton: "


-- TODO ops, preds, tests in parsemoc, parsecollection, run, test all alphabet chars
buildLBA :: String -> MoC
buildLBA alphabet = MoC (isValidLBAState alphabet) ops preds
    where
        -- shorthands
        apply = applyLBAOperation
        check = checkLBAPredicate

        ops opname = case opname of
            "NOP" -> Just id
            "L" -> Just $ apply moveLeft
            "R" -> Just $ apply moveRight
            ('W':s:[]) -> if s `elem` alphabet
                then Just $ apply (write s)
                else Nothing
            ('W':s:d:[]) -> if s `elem` alphabet && d `elem` "LNR"
                then Just $ apply (move d . write s)
                else Nothing
            "CRASH" -> Just $ apply (crash)
            _ -> Nothing

        preds predname = case predname of
            "LE" -> Just $ check (\(_, pos, _) -> pos <= (-1))
            "RE" -> Just $ check (\(tape, pos, _) -> pos >= length tape)
            ('=':s:[]) -> if s `elem` alphabet
                then Just $ check (isSymbol s)
                else Nothing
            _ -> Nothing


isLBAAlphabet :: String -> Bool
isLBAAlphabet "" = False
isLBAAlphabet alphabet = all (\s -> s `elem` vs) alphabet
    where
        vs = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "!%&()*+,-.;<>?@[]^_{|}~"


isValidLBAState :: String -> MachineState -> Bool
isValidLBAState alphabet ms
    | isNothing ms' = False -- ms needs to be readable
    | not $ isLBAAlphabet alphabet = False -- alphabet needs to be valid
    | pos < (-1) || pos >= length tape + 1 = False -- head needs to be in or "on" bounds
    | any (\s -> not $ s `elem` alphabet) tape =  False -- all symbols need to be valid
    | otherwise = True
    where
        ms'  = readMaybe ms :: Maybe LBAState
        (tape, pos, _) = fromJust ms'


-- reads machine state, applies operation and then returns string form of
-- machine state again
applyLBAOperation :: (LBAState -> LBAState) -> MachineState -> MachineState
applyLBAOperation op ms = show $ op (read ms :: LBAState)


-- reads machine state and checks predicate
checkLBAPredicate :: (LBAState -> Bool) -> MachineState -> Bool
checkLBAPredicate p ms = p (read ms :: LBAState)


isSymbol :: Char -> LBAState -> Bool
isSymbol s (tape, pos, _)
    | pos <= (-1) = False
    | pos >= length tape = False
    | otherwise = tape !! pos == s


moveLeft :: LBAState -> LBAState
moveLeft (tape, pos, crashed)
    | pos <= 0 = (tape, -1, crashed) -- position is left bound
    | otherwise = (tape, pos - 1, crashed)


moveRight :: LBAState -> LBAState
moveRight (tape, pos, crashed)
    | pos >= r = (tape, r + 1, crashed) -- position is right bound
    | otherwise = (tape, pos + 1, crashed)
    where
        r = length tape - 1 -- one position from right bound


move :: Char -> LBAState -> LBAState
move direction mstate = case direction of
    'L' -> moveLeft mstate
    'R' -> moveRight mstate
    _   -> mstate -- case 'N' or any other symbol


write :: Char -> LBAState -> LBAState
write s (tape, pos, crashed)
    | pos <= (-1) = (tape, -1, crashed)
    | pos >= rbound = (tape, rbound, crashed)
    | otherwise = (left ++ (s:right), pos, crashed)
    where
        rbound = length tape
        (left, (_:right)) = splitAt pos tape


crash :: LBAState -> LBAState
crash (tape, pos, _) = (tape, pos, True)
