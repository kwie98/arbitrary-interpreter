module ArbitraryInterpreter.Parse.ParseMoC
( parseMoC
) where

import ArbitraryInterpreter.Defs
import ArbitraryInterpreter.MoC.CounterMachine
import ArbitraryInterpreter.MoC.InvertedStackMachine
import ArbitraryInterpreter.MoC.StackMachine
import ArbitraryInterpreter.Parse.ReadProgramUtil

parseMoC :: String -> MoC
parseMoC text
    | not valid          = error $ err ++ "Bad format"
    | modelName == "cm"  = counterMachine args
    | modelName == "ism" = invertedStackMachine args
    | modelName == "sm"  = stackMachine args
    | otherwise          = error $ err ++ "Unknown model name"
    where
        args      = words . head $ prepareProgramText text -- split first line on whitespace
        valid     = let name = head args in (length $ name) > 1 && head name == '#'
        modelName = tail $ head args -- first arg, remove leading #
        err       = "Error parsing MoC definition: "
