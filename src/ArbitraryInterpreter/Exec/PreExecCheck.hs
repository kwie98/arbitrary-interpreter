module ArbitraryInterpreter.Exec.PreExecCheck
( preExecCheck
) where

import ArbitraryInterpreter.Defs
import ArbitraryInterpreter.Parse.ParseProgram
import ArbitraryInterpreter.Util.BDTVector
import Data.Maybe (isJust)
import qualified Data.HashMap.Strict as Map
import qualified Data.Vector as Vector (filter)

-- checks a parsed program for validity and executability. In detail, it does
-- the following checks on all BDTs of the program: Whether all leaves are
-- program state names, whether all branches are valid predicates, whether all
-- trees are complete (each branch has exactly two children) and whether all
-- vectors describing trees actually are trees (each node is reachable from the
-- root). Additionally, all the operations from each program state are checked
-- to be valid within the given MoC.
-- TODO (?):
-- state names can only consist of alphanumerics, predicates and operations can
-- additionally include special characters such as '+', '-', '*', '/', etc.
preExecCheck :: Program -> MoC -> Bool
preExecCheck prog moc =
    allLeavesStates trees states &&
    allBranchesPreds trees moc &&
    allOpsValid ops moc &&
    allTreesComplete trees &&
    allNodesReachable trees
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


-- assert that all BDTs are complete, meaning that every branch has exactly two
-- children
allTreesComplete :: [BDTVector] -> Bool
allTreesComplete [] = True
allTreesComplete (tree:trees)
    | isComplete tree = allTreesComplete trees
    | otherwise = error $ err ++ "Program has incomplete BDT: " ++ show tree


-- assert that the number of reachable nodes (decendants of the root node)
-- equals the amount of non-null values in the vector for each tree
allNodesReachable :: [BDTVector] -> Bool
allNodesReachable [] = True
allNodesReachable (tree:trees)
    | reachable == nodes = allNodesReachable trees
    | otherwise = error $ err ++ "Program has invalid BDT (unreachable nodes): " ++ show tree
    where
        reachable = countReachable tree
        nodes     = length $ Vector.filter (not . null) tree


isOp :: MoC -> OpName -> Bool
isOp moc s = isJust $ ops moc s


isPred :: MoC -> PredName -> Bool
isPred moc s = isJust $ preds moc s

err = "Error checking program: "
