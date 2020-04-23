module ArbitraryInterpreter.Parse.PreExecCheck
( preExecCheck
) where

import ArbitraryInterpreter.Defs
import ArbitraryInterpreter.Parse.ParseProgram
import ArbitraryInterpreter.Util.BDTVector
import Data.List (nub, (\\))
import Data.Maybe (isJust)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector (filter)
import Text.Read (readMaybe)

-- checks a parsed program for validity and executability. In detail, it does
-- the following checks on all BDTs of the program: Whether all leaves are
-- program state names, whether all branches are valid predicates, whether all
-- trees are complete (each branch has exactly two children) and whether all
-- vectors describing trees actually are trees (each node is reachable from the
-- root). Additionally, all the operations from each program state are checked
-- to be valid within the given MoC.
-- TODO: No loops back to start state! Always needs a start state!
-- TODO (?):
-- state names can only consist of alphanumerics, predicates and operations can
-- additionally include special characters such as '+', '-', '*', '/', etc.
preExecCheck :: ExtendedMoC -> Program -> Bool
preExecCheck (ExtendedMoC moc r _) prog =
    allLeavesStates trees states' &&
    allBranchesPreds trees moc &&
    allOpsValid ops moc r &&
    allTreesComplete trees &&
    allNodesReachable trees &&
    "Start" `elem` states
    where
        ops     = nub . map (fst) $ HashMap.elems prog
        trees   = nub . map (snd) $ HashMap.elems prog
        states  = nub $ HashMap.keys prog
        states' = ("End" : states) \\ ["Start"] -- reachable states


-- assert that all leaves in every given BDT are reachable states, meaning
-- defined states or "End", but not "Start"
allLeavesStates :: [BDTVector] -> [ProgramState] -> Bool
allLeavesStates [] _ = True
allLeavesStates (tree:trees) states = case length nonStates of
    0 -> allLeavesStates trees states
    1 -> error $ err ++ "State " ++ show nonStates ++ " is mentioned in BDT " ++ show tree ++ ", but is not defined state in the program"
    _ -> error $ err ++ "States " ++ show nonStates ++ " are mentioned in BDT " ++ show tree ++ ", but are not defined states in the program"
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


-- asserts that all operations from the program are valid operations in the
-- given MoC. Program calls begin with a $ and may include a permutation of up
-- to r registers, should r not be Nothing.
allOpsValid :: [OpName] -> MoC -> Maybe Int -> Bool
allOpsValid [] _ _ = True
allOpsValid (op:ops) moc r
    | isOp moc op = allOpsValid ops moc r
    | isValidPermutCall moc op r = allOpsValid ops moc r
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


-- checks whether permutations are allowed (program call in context of MoC with
-- registers) and whether permutation is correct (no repetitions, only
-- referencing existing registers)
isValidPermutCall :: MoC -> OpName -> Maybe Int -> Bool
isValidPermutCall moc op@('$':_) (Just r) = case p of
    Just permut ->
        maximum permut <= r &&
        (length permut) == (length $ nub permut) &&
        isOp moc opcode
    Nothing -> False
    where
        p = sequence . map (readMaybe) . tail $ words op :: Maybe [Int]
        opcode = head $ words op
isValidPermutCall _ _ _ = False


isPred :: MoC -> PredName -> Bool
isPred moc s = isJust $ preds moc s

err = "Error checking program: "
