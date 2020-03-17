module ArbitraryInterpreter.Exec.RunProgram
( evalSafe
, evalBDT
) where

import ArbitraryInterpreter.Defs
import ArbitraryInterpreter.Exec.PreExecCheck
import ArbitraryInterpreter.Util.BDTVector
import qualified Data.HashMap.Strict as Map
import qualified Data.Vector as Vector

-- sequence of predicates that were evaluated to reach the next program state
type PredicateSequence = [String]

-- Execute program for one step, not running preExecCheck
eval :: MoC -> Program -> ProgramState -> MachineState -> (ProgramState, MachineState, PredicateSequence)
eval moc program pstate mstate = (pstate', mstate', preds)
    where
        -- get new program state and precicate sequence by evaluating BDT of old program state on old machine state
        (pstate', preds) = case Map.lookup pstate program of
            Nothing       -> error $ err ++ "State " ++ pstate ++ " is not defined in the given program"
            Just (_, bdt) -> evalBDT moc bdt mstate

        -- lookup the name of the next operation
        opName' = case Map.lookup pstate' program of
            Nothing     -> error $ err ++ "State " ++ pstate' ++ " is not defined in the given program"
            Just (s, _) -> s

        -- extract the actual operation from the MoC
        op' = case ops moc opName' of
            Nothing -> error $ err ++ "Operation " ++ opName' ++ " is not defined in the given MoC"
            Just f  -> f

        -- get new machine state by executing the operation of new program state on old machine state
        mstate' = op' mstate


-- Executes program for one step after running preExecCheck
-- Should the program not be valid, an error is thrown
evalSafe :: MoC -> Program -> ProgramState -> MachineState -> (ProgramState, MachineState, PredicateSequence)
evalSafe moc program pstate mstate = case preExecCheck program moc of
    True  -> eval moc program pstate mstate
    False -> error $ err ++ "Invalid program"


-- Evaluate a given BDT using a specific MoC and machine state
evalBDT :: MoC -> BDTVector -> MachineState -> (ProgramState, PredicateSequence)
evalBDT moc bdt mstate = (last path, init path)
    where
        path = case validState moc mstate of
            True  -> evalBDT' moc bdt 0 mstate
            False -> error $ err ++ "Invalid machine state: " ++ mstate


-- preExecCheck asserts that all branches are valid predicates and that all
-- leaves are valid states. List of strings consists of all checked predicates
-- and ends with the new program state
evalBDT' :: MoC -> BDTVector -> Int -> MachineState -> [String]
evalBDT' moc bdt i mstate
    | isPState           = [curNode]
    | isPred && predTrue = curNode : evalBDT' moc bdt (i * 2 + 1) mstate
    | isPred             = curNode : evalBDT' moc bdt (i * 2 + 2) mstate
    | otherwise          = error $ err ++ "invalid node in BDT reached: " ++ show bdt ++ " (index: " ++ show i ++ ")"
    where
        curNode  = bdt Vector.! i
        isPState = isLeaf bdt i
        isPred   = isBranch bdt i
        predTrue = case preds moc curNode of
            Nothing -> error $ err ++ "invalid predicate in BDT reached: " ++ show curNode
            Just f  -> f mstate


-- run a program for x steps / until completion, first asserting that the program is valid for the given MoC
-- run  :: MoC -> (Maybe Int) -> Program -> MachineState -> (ProgramState, MachineState)

err = "Error running program: "