module ArbitraryInterpreter.MoC.LinearBoundedAutomaton
( linearBoundedAutomaton
) where

import ArbitraryInterpreter.Defs
import Data.Maybe (fromJust, isNothing)
import Text.Read (readMaybe)

-- checks the given arguments before passing them to MoC builder, precisely:
-- number of arguments is 1
-- arg0 is a string description of a valid TM alphabet (as checked by other function)
linearBoundedAutomaton :: [String] -> MoC
linearBoundedAutomaton args
    | length args /= 1 =
        error $ err ++ "Incorrect number of arguments"
    | badAlphabet =
        error $ err ++ "Expected valid LBAMOC alphabet, got: " ++ args !! 0 ++
        " (Alphabet can only include these symbols: " ++ validSymbols ++ ")"
    | otherwise =
        buildLBA (fromJust arg0)
    where
        arg0 = readMaybe (args !! 0) :: Maybe String
        badAlphabet = (isLBAAlphabet <$> arg0) /= Just True
        err = "Error parsing arguments for LBAMOC: "


buildLBA :: String -> MoC
buildLBA alphabet = MoC (isValidLBAState alphabet) ops preds
    where
        -- shorthands
        apply = applyLBAOperation
        check = checkLBAPredicate

        ops opname = case opname of
            "L" -> Just $ apply moveLeft
            "R" -> Just $ apply moveRight
            ('W':s:[]) -> if s `elem` alphabet
                then Just $ apply (write s)
                else Nothing
            ('W':s:d:[]) -> if s `elem` alphabet && d `elem` "LNR"
                then Just $ apply (move d . write s)
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
isLBAAlphabet alphabet = all (\s -> s `elem` validSymbols) alphabet


isValidLBAState :: String -> MachineState -> Maybe String
isValidLBAState alphabet ms
    | isNothing ms' = -- ms needs to be readable
        Just "Machine state needs to have format (String, Int)"
    | pos < (-1) || pos >= length tape + 1 = -- head needs to be in or "on" bounds
        Just "Tape head cannot be out of bounds"
    | any (\s -> not $ s `elem` alphabet) tape = -- all symbols need to be valid
        Just "Tape can only consist of symbols from the alphabet"
    | otherwise = Nothing
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


move :: Char -> (String, Int) -> (String, Int)
move direction mstate = case direction of
    'L' -> moveLeft mstate
    'R' -> moveRight mstate
    _   -> mstate -- case 'N' or any other symbol


write :: Char -> (String, Int) -> (String, Int)
write s (tape, pos)
    | pos <= (-1) = (tape, -1)
    | pos >= rbound = (tape, rbound)
    | otherwise = (left ++ (s:right), pos)
    where
        rbound = length tape
        (left, (_:right)) = splitAt pos tape
