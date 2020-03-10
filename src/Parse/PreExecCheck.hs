module Parse.PreExecCheck (
preExecCheck
) where

import Parse.ParseProgram
import MoC.MoC

preExecCheck :: Program -> MoC -> Bool
preExecCheck _ _ = True
