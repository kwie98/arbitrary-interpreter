module Main where

import ArbitraryInterpreter.Parse.ParseMoC
import ArbitraryInterpreter.Parse.ParseProgram
import ArbitraryInterpreter.Exec.PreExecCheck

t = "    R1L\n\
    \        R1A\n\
    \            R1B\n\
    \                R1C\n\
    \                    UN\n\
    \                    Z2C\n\
    \                Z2B\n\
    \            Z2A\n\
    \        End\n"

t2 = "    Z3\n"

program2 = "#sm, 2 registers\n\
           \Z2C / R2FC:\n\
           \    Z3\n"

program3 = "#sm, 2 registers"


main :: IO ()
main = do
    prog <- getContents
    print $ preExecCheck (parseProgram prog) (parseMoC prog)
