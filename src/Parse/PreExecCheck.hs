module Parse.PreExecCheck (
preExecCheck
) where

import Parse.ParseProgram
import MoC.MoC
import Util.BDTVector
import qualified Data.HashMap.Strict as Map

-- after parsing all trees, check that all leaves are existing state names and
--   all inner nodes are predicates
-- also check that all operations are valid in the given MoC
preExecCheck :: Program -> MoC -> Bool
preExecCheck prog moc =
    allLeavesStates trees states &&
    allInnerPreds trees moc &&
    allOpsValid ops moc
    where
        ops    = map (fst) $ Map.elems prog
        trees  = map (snd) $ Map.elems prog
        states = Map.keys prog

allLeavesStates :: [BDTVector] -> [String] -> Bool
allLeavesStates trees states = False


allInnerPreds :: [BDTVector] -> MoC -> Bool
allInnerPreds trees moc = False


allOpsValid :: [String] -> MoC -> Bool
allOpsValid ops moc = False
