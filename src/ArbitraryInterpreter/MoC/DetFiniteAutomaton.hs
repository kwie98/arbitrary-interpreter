module ArbitraryInterpreter.MoC.DetFiniteAutomaton
( finiteStateAutomaton
) where

import ArbitraryInterpreter.Defs
import Data.Maybe (fromJust, isNothing)
import Text.Read (readMaybe)

-- checks the given arguments before passing them to MoC builder, precisely:
-- number of arguments is 1
-- arg0 is a string description of a valid TM alphabet (as checked by other function)
finiteStateAutomaton :: [String] -> MoC
finiteStateAutomaton args
    | length args /= 1 =
        error $ err ++ "Incorrect number of arguments"
    | badAlphabet =
        error $ err ++ "Expected valid DFAMOC alphabet, got: " ++ args !! 0 ++
        " (Alphabet can only include these symbols: " ++ validSymbols ++ ")"
    | otherwise =
        buildDFA (fromJust arg0)
    where
        arg0 = readMaybe (args !! 0) :: Maybe String
        badAlphabet = (isDFAAlphabet <$> arg0) /= Just True
        err = "Error parsing arguments for DFAMOC: "


buildDFA :: String -> MoC
buildDFA alphabet = MoC (isValidDFAState alphabet) ops preds
    where
        -- shorthands
        apply = applyDFAOperation
        check = checkDFAPredicate

        ops opname = case opname of
            "NXT" -> Just $ apply (\(input, acc) -> (tail' input, acc))
            "ACC" -> Just $ apply (\(_, _) -> ("", True))
            _ -> Nothing

        preds predname = case predname of
            -- empty check:
            "I=" -> Just $ check (\(input, _) -> input == "")
            -- symbol check for first elemens of input:
            ('I':'=':x:[]) -> if x `elem` alphabet
                then Just $ check (inputIs x)
                else Nothing
            _ -> Nothing


isDFAAlphabet :: String -> Bool
isDFAAlphabet "" = False
isDFAAlphabet alphabet = all (\s -> s `elem` validSymbols) alphabet


isValidDFAState :: String -> MachineState -> Maybe String
isValidDFAState alphabet ms
    | isNothing ms' =
        Just "Machine state needs to have format (String, Bool)"
    | any (\s -> not $ s `elem` alphabet) input =
        Just "Input can only consist of symbols from the alphabet"
    | otherwise = Nothing
    where
        ms'  = readMaybe ms :: Maybe (String, Bool)
        (input, accepted) = fromJust ms'


-- reads machine state, applies operation and then returns string form of
-- machine state again
applyDFAOperation :: ((String, Bool) -> (String, Bool)) -> MachineState -> MachineState
applyDFAOperation op ms = show $ op (read ms :: (String, Bool))


-- reads machine state and checks predicate
checkDFAPredicate :: ((String, Bool) -> Bool) -> MachineState -> Bool
checkDFAPredicate p ms = p (read ms :: (String, Bool))


-- remove first element, if empty list then just return empty list
tail' :: [a] -> [a]
tail' [] = []
tail' (_:xs) = xs


inputIs :: Char -> (String, Bool) -> Bool
inputIs _ (""    , _) = False
inputIs s ((fi:_), _) = s == fi
