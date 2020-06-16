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
    | length args /= 1 = error $ err ++ "Incorrect number of arguments"
    | badAlphabet = error $ err ++ "Expected valid DFA alphabet, got: " ++ args !! 0
    | otherwise = buildDFA (fromJust arg0)
    where
        arg0 = readMaybe (args !! 0) :: Maybe String
        badAlphabet = (isDFAAlphabet <$> arg0) /= Just True
        err = "Error parsing arguments for finite state automaton: "


-- TODO tests in parsemoc, parsecollection, run
buildDFA :: String -> MoC
buildDFA alphabet = MoC (isValidDFAState alphabet) ops preds
    where
        -- shorthands
        apply = applyDFAOperation
        check = checkDFAPredicate

        ops opname = case opname of
            "NOP" -> Just id
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


-- same alphabet as TM, LBA, PDA
isDFAAlphabet :: String -> Bool
isDFAAlphabet "" = False
isDFAAlphabet alphabet = all (\s -> s `elem` vs) alphabet
    where
        vs = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "!%&()*+,-.;<>?@[]^_{|}~"


isValidDFAState :: String -> MachineState -> Bool
isValidDFAState alphabet ms
    | isNothing ms' = False -- ms needs to be readable
    | not $ isDFAAlphabet alphabet = False -- alphabet needs to be valid
    | any (\s -> not $ s `elem` alphabet) input = False
    | otherwise = True
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
