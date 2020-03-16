module Main where

import Parse.ParseMoC
import Parse.ParseProgram
import Parse.PreExecCheck

program1 = "#sm, 2 registers\n\
\Start:\n\
\    R2L\n\
\        Z1\n\
\        R1L\n\
\            R1A\n\
\                R1B\n\
\                    R1C\n\
\                        UN\n\
\                        Z2C\n\
\                    Z2B\n\
\                Z2A\n\
\            End    \n\
\Z2A / R2FA:\n\
\\tZ3\n\
\Z2B / R2FB:\n\
\    Z3\n\
\Z3 / R1E:\n\
\  R1L\n\
\    R1A\n\
\      R1B\n\
\        R1C\n\
\          UN\n\
\          Z2C\n\
\        Z2B\n\
\      Z2A\n\
\    End           \n\
\Z1 / R2E:\n\
\    R2L\n\
\        Z1\n\
\        R1L\n\
\            R1A\n\
\                R1B\n\
\                    R1C\n\
\                        UN\n\
\                        Z2C\n\
\                    Z2B\n\
\                Z2A\n\
\            End     \n\
\            \n\
\Z2C / R2FC:\n\
\    Z3\n\
\UN/ NOP:\n\
\      End\n"

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
