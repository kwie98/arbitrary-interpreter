module ArbitraryInterpreter.Exec.PreExecCheck
( preExecCheck
) where

import ArbitraryInterpreter.Defs
import ArbitraryInterpreter.Parse.ParseProgram
import ArbitraryInterpreter.Util.BDTVector
import Data.Maybe (isJust)
import qualified Data.HashMap.Strict as Map

-- after parsing all trees, check that all leaves are existing state names and
-- all inner nodes are predicates
-- also check that all operations are valid in the given MoC
-- TODO check structure of all BDTs? (everything reachable, always two children or none)
preExecCheck :: Program -> MoC -> Bool
preExecCheck prog moc =
    allLeavesStates trees states &&
    allBranchesPreds trees moc &&
    allOpsValid ops moc
    where
        ops    = map (fst) $ Map.elems prog
        trees  = map (snd) $ Map.elems prog
        states = "End" : Map.keys prog


-- assert that all leaves in every given BDT are included in the given state list
allLeavesStates :: [BDTVector] -> [ProgramState] -> Bool
allLeavesStates [] _ = True
allLeavesStates (tree:trees) states
    | null nonStates = allLeavesStates trees states
    | otherwise = error $ err ++ "State(s) " ++ show nonStates ++ " are mentioned in BDT " ++ show tree ++ ", but are not defined states in the program"
    where
        nonStates = filter (\leaf -> not $ leaf `elem` states) (leaves tree)


-- assert that all branches in every given BDT are valid predicates in the given MoC
allBranchesPreds :: [BDTVector] -> MoC -> Bool
allBranchesPreds [] _ = True
allBranchesPreds (tree:trees) moc
    | null nonPreds = allBranchesPreds trees moc
    | otherwise = error $ err ++ "Predicate(s) " ++ show nonPreds ++ " are mentioned in BDT" ++ show tree ++ ", but are not defined in the given MoC"
    where
        nonPreds = filter (\branch -> not $ isPred moc branch) (branches tree)


-- assert that all operations from the program are valid operations in the given MoC
allOpsValid :: [OpName] -> MoC -> Bool
allOpsValid [] _ = True
allOpsValid (op:ops) moc
    | isOp moc op = allOpsValid ops moc
    | otherwise = error $ err ++ "Program mentions invalid operation " ++ op


isOp :: MoC -> OpName -> Bool
isOp moc s = isJust $ ops moc s


isPred :: MoC -> PredName -> Bool
isPred moc s = isJust $ preds moc s

err = "Error checking program: "
