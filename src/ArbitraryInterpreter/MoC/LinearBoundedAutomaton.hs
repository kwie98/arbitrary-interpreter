module ArbitraryInterpreter.MoC.LinearBoundedAutomaton where

import ArbitraryInterpreter.Defs
import Data.Maybe (fromJust, isNothing)
import Text.Read (readMaybe)

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
            _ -> Nothing

        preds predname = case predname of
            "LE" -> Just $ check (\(_, pos) -> pos <= (-1))
            "RE" -> Just $ check (\(tape, pos) -> pos >= length tape)
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
        ms'  = readMaybe ms :: Maybe (String, Int)
        (tape, pos) = fromJust ms'


-- reads machine state, applies operation and then returns string form of
-- machine state again
applyLBAOperation :: ((String, Int) -> (String, Int)) -> MachineState -> MachineState
applyLBAOperation op ms = show $ op (read ms :: (String, Int))


-- reads machine state and checks predicate
checkLBAPredicate :: ((String, Int) -> Bool) -> MachineState -> Bool
checkLBAPredicate p ms = p (read ms :: (String, Int))


isSymbol :: Char -> (String, Int) -> Bool
isSymbol s (tape, pos)
    | pos <= (-1) = False
    | pos >= length tape = False
    | otherwise = tape !! pos == s


moveLeft :: (String, Int) -> (String, Int)
moveLeft (tape, pos)
    | pos <= 0 = (tape, -1) -- position is left bound
    | otherwise = (tape, pos - 1)


moveRight :: (String, Int) -> (String, Int)
moveRight (tape, pos)
    | pos >= r = (tape, r + 1) -- position is right bound
    | otherwise = (tape, pos + 1)
    where
        r = length tape - 1 -- one position from right bound


write :: Char -> (String, Int) -> (String, Int)
write s (tape, pos)
    | pos <= (-1) = (tape, -1)
    | pos >= rbound = (tape, rbound)
    | otherwise = (left ++ (s:right), pos)
    where
        rbound = length tape
        (left, (_:right)) = splitAt pos tape
