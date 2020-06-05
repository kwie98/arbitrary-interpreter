module ArbitraryInterpreter.MoC.PushdownAutomaton
( pushdownAutomaton
) where

import ArbitraryInterpreter.Defs
import Data.Maybe (fromJust, isNothing)
import Text.Read (readMaybe)

-- checks the given arguments before passing them to MoC builder, precisely:
-- number of arguments is 1
-- arg0 is a string description of a valid TM alphabet (as checked by other function)
pushdownAutomaton :: [String] -> MoC
pushdownAutomaton args
    | length args /= 1 = error $ err ++ "Incorrect number of arguments"
    | badAlphabet = error $ err ++ "Expected valid PDA alphabet, got: " ++ args !! 0
    | otherwise = buildPDA (fromJust arg0)
    where
        arg0 = readMaybe (args !! 0) :: Maybe String
        badAlphabet = (isPDAAlphabet <$> arg0) /= Just True
        err = "Error parsing arguments for pushdown automaton: "


-- TODO tests in parsemoc, parsecollection, run
buildPDA :: String -> MoC
buildPDA alphabet = MoC (isValidPDAState alphabet) ops preds
    where
        -- shorthands
        apply = applyPDAOperation
        check = checkPDAPredicate

        ops opname = case opname of
            "NOP" -> Just id
            "ACC" -> Just $ apply (\(_, stack, _) -> ("", stack, True))
            ('W':str) -> if all (\s -> s `elem` alphabet) str
                then Just $ apply (write str)
                else Nothing
            _ -> Nothing

        preds predname = case predname of
            -- empty checks:
            "S=" -> Just $ check (\(_, stack, _) -> stack == "")
            "I=" -> Just $ check (\(input, _, _) -> input == "")
            -- symbol checks for first elemens of stack and input:
            ('S':'=':x:[]) -> if x `elem` alphabet
                then Just $ check (stackIs x)
                else Nothing
            ('I':'=':x:[]) -> if x `elem` alphabet
                then Just $ check (inputIs x)
                else Nothing
            _ -> Nothing


-- same alphabet as TM, LBA
isPDAAlphabet :: String -> Bool
isPDAAlphabet "" = False
isPDAAlphabet alphabet = all (\s -> s `elem` vs) alphabet
    where
        vs = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "!%&()*+,-.;<>?@[]^_{|}~"


isValidPDAState :: String -> MachineState -> Bool
isValidPDAState alphabet ms
    | isNothing ms' = False -- ms needs to be readable
    | not $ isPDAAlphabet alphabet = False -- alphabet needs to be valid
    | any (\s -> not $ s `elem` alphabet) input = False
    | any (\s -> not $ s `elem` alphabet) stack = False
    | otherwise = True
    where
        ms'  = readMaybe ms :: Maybe (String, String, Bool)
        (input, stack, accepted) = fromJust ms'


-- reads machine state, applies operation and then returns string form of
-- machine state again
applyPDAOperation :: ((String, String, Bool) -> (String, String, Bool)) -> MachineState -> MachineState
applyPDAOperation op ms = show $ op (read ms :: (String, String, Bool))


-- reads machine state and checks predicate
checkPDAPredicate :: ((String, String, Bool) -> Bool) -> MachineState -> Bool
checkPDAPredicate p ms = p (read ms :: (String, String, Bool))


write :: String -> (String, String, Bool) -> (String, String, Bool)
write _   ("", stack, acc) = ("", stack, acc) -- do nothing on empty input
write str ((_:input), "", acc) = (input, str, acc)
write str ((_:input), (_:stack), acc) = (input, str ++ stack, acc)


stackIs :: Char -> (String, String, Bool) -> Bool
stackIs _ (_, ""    , _) = False
stackIs s (_, (fs:_), _) = s == fs


inputIs :: Char -> (String, String, Bool) -> Bool
inputIs _ (""    , _, _) = False
inputIs s ((fi:_), _, _) = s == fi
