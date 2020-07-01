module ArbitraryInterpreter.Parse.ParseMoC
( parseMoC
, addOperation
) where

import ArbitraryInterpreter.Defs
import ArbitraryInterpreter.MoC.CounterMachine
import ArbitraryInterpreter.MoC.InvertedStackMachine
import ArbitraryInterpreter.MoC.StackMachine
import ArbitraryInterpreter.MoC.TuringMachine
import ArbitraryInterpreter.MoC.LinearBoundedAutomaton
import ArbitraryInterpreter.MoC.RTDetPushdownAutomaton
import ArbitraryInterpreter.MoC.DetFiniteAutomaton
import ArbitraryInterpreter.MoC.Permuters
import ArbitraryInterpreter.MoC.Prettifiers
import Data.Char (toLower)
import Data.Maybe (isJust)

parseMoC :: String -> ExtendedMoC
parseMoC line
    | not valid = error $ err ++ "Bad format"
    | otherwise = addOperation m "NOP" (id)
    where
        els       = words line -- split line on whitespace
        valid     = head els == "#MOC"
        args      = tail $ tail els
        modelName = map (toLower) (els !! 1)
        numRegs   = read $ els !! 2 :: Int
        err       = "Error parsing MoC definition: "
        m = case modelName of
            "cmmoc"   -> ExtendedMoC (counterMachine args)
                (Just (MoCInfo numRegs (Just permuteInts) (Just prettifyInts)))
            "ismmoc"  -> ExtendedMoC (invertedStackMachine args)
                (Just (MoCInfo numRegs (Just permuteStrings) (Just prettifyStrings)))
            "smmoc"   -> ExtendedMoC (stackMachine args)
                (Just (MoCInfo numRegs (Just permuteStrings) (Just prettifyStrings)))
            "tmmoc"   -> ExtendedMoC (turingMachine args) Nothing
            "lbamoc"  -> ExtendedMoC (linearBoundedAutomaton args) Nothing
            "rdpamoc" -> ExtendedMoC (pushdownAutomaton args) Nothing
            "dfamoc"  -> ExtendedMoC (finiteStateAutomaton args) Nothing
            _         -> error $ err ++ "Unknown model name"


-- adds a given operation to a MoC. Throws an error if the given operation name
-- already describes an operation. Used for program calls.
addOperation :: ExtendedMoC -> OpName -> (MachineState -> MachineState) -> ExtendedMoC
addOperation (ExtendedMoC oldmoc moci) opname op = case ops oldmoc opname of
    Just _  -> error $ err ++ "Operation " ++ opname ++ " already exists"
    Nothing -> ExtendedMoC (MoC (validState oldmoc) ops' (preds oldmoc)) moci -- validState and preds unchanged
    where
        ops' str
            | str == opname           = Just op
            | isJust $ ops oldmoc str = ops oldmoc str
            | otherwise               = Nothing

        err = "Error adding an operation to model of computation: "
