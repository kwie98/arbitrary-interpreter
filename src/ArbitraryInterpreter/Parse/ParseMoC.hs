module ArbitraryInterpreter.Parse.ParseMoC
( parseMoC
, addOperation
) where

import ArbitraryInterpreter.Defs
import ArbitraryInterpreter.MoC.CounterMachine
import ArbitraryInterpreter.MoC.InvertedStackMachine
import ArbitraryInterpreter.MoC.StackMachine
import Data.Char (toLower)
import Data.Maybe (isJust)

parseMoC :: String -> ExtendedMoC
parseMoC line
    | not valid = error $ err ++ "Bad format"
    | modelName == "cm" =
        ExtendedMoC (counterMachine args) (Just numRegs) id
    | modelName == "ism" =
        ExtendedMoC (invertedStackMachine args) (Just numRegs) id
    | modelName == "sm" =
        ExtendedMoC (stackMachine args) (Just numRegs) id
    | otherwise = error $ err ++ "Unknown model name"
    where
        els       = words line -- split line on whitespace
        valid     = head els == "#MOC"
        args      = tail els
        modelName = map (toLower) (els !! 1)
        numRegs   = read $ els !! 2 :: Int
        err       = "Error parsing MoC definition: "


-- adds a given operation to a MoC. Throws an error if the given operation name
-- already describes an operation.
addOperation :: ExtendedMoC -> OpName -> (MachineState -> MachineState) -> ExtendedMoC
addOperation (ExtendedMoC oldmoc r p) opname op = case ops oldmoc opname of
    Just _  -> error $ err ++ "Operation " ++ opname ++ " already exists"
    Nothing -> ExtendedMoC (MoC (validState oldmoc) ops' (preds oldmoc)) r p -- validState and preds unchanged
    where
        ops' str
            | str == opname           = Just op
            | isJust $ ops oldmoc str = ops oldmoc str
            | otherwise               = Nothing

        err = "Error adding an operation to model of computation: "
