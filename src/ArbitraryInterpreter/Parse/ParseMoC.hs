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

-- TODO #MOC MODELNAME ARG1 ARG2 ... ARGN
parseMoC :: String -> MoC
parseMoC line
    | not valid          = error $ err ++ "Bad format"
    | modelName == "cm"  = counterMachine args
    | modelName == "ism" = invertedStackMachine args
    | modelName == "sm"  = stackMachine args
    | otherwise          = error $ err ++ "Unknown model name"
    where
        els       = words line -- split line on whitespace
        valid     = head els == "#MOC"
        modelName = map (toLower) (els !! 1)
        args      = tail els
        err       = "Error parsing MoC definition: "


-- adds a given operation to a MoC. Throws an error if the given operation name
-- already describes an operation.
addOperation :: MoC -> OpName -> (MachineState -> MachineState) -> MoC
addOperation oldmoc opname op = case ops oldmoc opname of
    Just _  -> error $ err ++ "Operation " ++ opname ++ " already exists"
    Nothing -> MoC (validState oldmoc) ops' (preds oldmoc) -- validState and preds unchanged
    where
        ops' str
            | str == opname           = Just op
            | isJust $ ops oldmoc str = ops oldmoc str
            | otherwise               = Nothing

        err = "Error adding an operation to model of computation: "
