module Parse.ParseMoC (
parseMoC
) where

import MoC.MoC
import MoC.CounterMachine
import MoC.StackMachine
import Parse.ReadProgramUtil

parseMoC :: String -> MoC
parseMoC text
    | not valid         = error $ err ++ "Bad format"
    | modelName == "cm" = counterMachine ((read $ args !! 1) :: Int)
    | modelName == "sm" = stackMachine ((read $ args !! 1) :: Int) (args !! 2)
    | otherwise         = error $ err ++ "Unknown model name"
    where
        args      = words . head $ prepareProgramText text -- split first line on whitespace
        valid     = let name = head args in (length $ name) > 1 && head name == '#'
        modelName = tail $ head args -- first arg, remove leading #
        err       = "Error parsing MoC definition: "
