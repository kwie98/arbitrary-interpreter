module Parse.PreExecCheck (
preExecCheck
) where

import Parse.ParseProgram
import MoC.MoC


-- after parsing all trees, check that all leafs are existing state names and
--   all inner nodes are predicates
-- also check that all operations are valid in the given MoC
preExecCheck :: Program -> MoC -> Bool
preExecCheck prog moc = False
