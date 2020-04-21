module ArbitraryInterpreter.Exec.RunProgram
( eval
, evalBDT
, run
, runPrintTrace
) where

import ArbitraryInterpreter.Defs
import ArbitraryInterpreter.Util.BDTVector
import qualified Data.HashMap.Strict as Map
import Data.List (intersperse)
import qualified Data.Vector as Vector
import Text.Read (readMaybe)

-- sequence of predicates that were evaluated to reach the next program state
-- together with their truth value
type PredicateSequence = [(String, Bool)]

-- Executes program for one step. Throws an error when trying to evaluate "End" state
eval :: MoC -> Program -> ProgramState -> MachineState -> (ProgramState, MachineState, PredicateSequence)
eval moc program pstate mstate = case pstate' of
    "End" -> (pstate', mstate, preds)
    _     -> (pstate', mstate', preds)
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


-- Evaluate a given BDT using a specific MoC and machine state
evalBDT :: MoC -> BDTVector -> MachineState -> (ProgramState, PredicateSequence)
evalBDT moc bdt mstate = (fst $ last path, init path)
    where
        path = case validState moc mstate of
            True  -> evalBDT' moc bdt 0 mstate
            False -> error $ err ++ "Invalid machine state: " ++ mstate


-- preExecCheck asserts that all branches are valid predicates and that all
-- leaves are valid states. List of strings consists of all checked predicates
-- and ends with the new program state. First branch corresponds to predicate
-- being false, second branch corresponds to predicate being true
evalBDT' :: MoC -> BDTVector -> Int -> MachineState -> PredicateSequence
evalBDT' moc bdt i mstate
    | isPState           = [(curNode, True)]
    | isPred && predTrue = (curNode, True) : evalBDT' moc bdt (i * 2 + 2) mstate
    | isPred             = (curNode, False) : evalBDT' moc bdt (i * 2 + 1) mstate
    | otherwise          = error $ err ++ "invalid node in BDT reached: " ++ show bdt ++ " (index: " ++ show i ++ ")"
    where
        curNode  = bdt Vector.! i
        isPState = isLeaf bdt i
        isPred   = isBranch bdt i
        predTrue = case preds moc curNode of
            Nothing -> error $ err ++ "invalid predicate in BDT reached: " ++ show curNode
            Just f  -> f mstate


-- run a program for x steps
run :: Maybe Int -> MoC -> Program -> MachineState -> (ProgramState, MachineState)
run i moc program mstate = run' i moc program "Start" mstate


run' :: Maybe Int -> MoC -> Program -> ProgramState -> MachineState -> (ProgramState, MachineState)
run' i moc program pstate mstate
    | ((> 0) <$> i) == Just False = (pstate, mstate) -- no steps left
    | pstate == "End"             = (pstate, mstate) -- end state reached
    | otherwise                   = run' i' moc program pstate' mstate'
    where
        i' = (subtract 1) <$> i
        (pstate', mstate', _) = eval moc program pstate mstate


runPrintTrace :: Maybe Int -> MoC -> Program -> MachineState -> IO ()
runPrintTrace i moc program mstate = do
    putStrLn "program state,machine state,predicate sequence"
    runPrintTrace' i moc program "Start" mstate []


runPrintTrace' :: Maybe Int -> MoC -> Program -> ProgramState -> MachineState -> PredicateSequence -> IO ()
runPrintTrace' i moc program pstate mstate trace =
    if (((> 0) <$> i) == Just False || pstate == "End") -- no steps left or end state reached
        then do
            putCSV pstate mstate trace
            -- putStr . remnewline $ pstate ++ " " ++ mstate ++ " " ++ show trace
            -- putChar '\n'
        else do
            putCSV pstate mstate trace
            -- putStr . remnewline $ pstate ++ " " ++ mstate ++ " " ++ show trace
            -- putChar '\n'
            let i' = (subtract 1) <$> i
                (pstate', mstate', trace') = eval moc program pstate mstate
            runPrintTrace' i' moc program pstate' mstate' trace'


-- TODO this only works for specific machine state formats, namely [Int] and [String]!
putCSV :: ProgramState -> MachineState -> PredicateSequence -> IO ()
putCSV pstate mstate trace =
    putStrLn . remnewline $ pstate ++ "," ++ mstate' ++ "," ++ trace'
    where
        regsS   = readMaybe mstate :: Maybe [String]
        regsI   = readMaybe mstate :: Maybe [Int]
        mstate' = case (regsS, regsI) of
            (Just a, _) -> concat $ intersperse " " $ map (show) a
            (_, Just b) -> concat $ intersperse " " $ map (show) b
            _           -> error "Error printing csv"
        trace'  = concat $ intersperse " -> " $ map (\(p, b) -> p ++ " " ++ show b) trace


remnewline :: String -> String
remnewline s = filter (\c -> c /= '\n') s


err = "Error running program: "
