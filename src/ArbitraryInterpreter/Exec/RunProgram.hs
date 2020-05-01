module ArbitraryInterpreter.Exec.RunProgram
( eval
, evalBDT
, run
, runPrintTrace
) where

import ArbitraryInterpreter.Defs
import ArbitraryInterpreter.Util.BDTVector
import qualified Data.HashMap.Strict as Map
import Data.List (intersperse, sortOn, (\\))
import Data.Maybe (fromJust)
import qualified Data.Vector as Vector
import Text.Read (readMaybe)

-- sequence of predicates that were evaluated to reach the next program state
-- together with their truth value
type PredicateSequence = [(String, Bool)]

-- Executes program for one step. Throws an error when trying to evaluate "End" state
eval :: ExtendedMoC -> Program -> ProgramState -> MachineState -> (ProgramState, MachineState, PredicateSequence)
eval (ExtendedMoC moc moci) program pstate mstate = case pstate' of
    "End" -> (pstate', mstate, [])
    _     -> (pstate', mstate', preds)
    where
        -- get new program state by evaluating BDT of given program state on given machine state
        (pstate', preds) = case Map.lookup pstate program of
            Nothing       -> error $ err ++ "State " ++ pstate ++ " is not defined in the given program"
            Just (_, bdt) -> evalBDT moc bdt mstate

        -- lookup the name of the next operation
        (opName', permut') = (head w, map read $ tail w :: [Int])
            where
                w = words $ case Map.lookup pstate' program of
                    Nothing     -> error $ err ++ "State " ++ pstate' ++ " is not defined in the given program"
                    Just (s, _) -> s

        -- extract the actual operation from the MoC
        op' = case ops moc opName' of
            Nothing -> error $ err ++ "Operation " ++ opName' ++ " is not defined in the given MoC"
            Just f  -> f

        -- get new machine state by executing the operation of new program state
        -- on given machine state, permuting the machine state before and after
        -- if specified
        mstate' = case (moci, permut') of
            -- no permutation:
            (_, []) -> op' mstate
            -- permutation given, permutations allowed by MoC:
            (Just (MoCInfo r (Just permute) _), _) -> permute invPermut . op' $ permute fullPermut mstate
                where
                    invPermut = fill permut'
                    fullPermut = map fst $ sortOn snd $ zip [1..r] invPermut
            -- permutation given, not allowed by MoC:
            _ -> error $ err ++ "Operation with register permutation in program for non-register machine"


-- completes a list representing a permutation (one-line notation) TODO MAY NEED UPPER LIMIT
fill :: [Int] -> [Int]
fill xs = xs ++ ([1..] \\ xs)


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
run :: Maybe Int -> ExtendedMoC -> Program -> MachineState -> (ProgramState, MachineState)
run i xmoc program mstate = run' i xmoc program "Start" mstate


run' :: Maybe Int -> ExtendedMoC -> Program -> ProgramState -> MachineState -> (ProgramState, MachineState)
run' i xmoc program pstate mstate
    | ((> 0) <$> i) == Just False = (pstate, mstate) -- no steps left
    | pstate == "End"             = (pstate, mstate) -- end state reached
    | otherwise                   = run' i' xmoc program pstate' mstate'
    where
        i' = (subtract 1) <$> i
        (pstate', mstate', _) = eval xmoc program pstate mstate


runPrintTrace :: Maybe Int -> ExtendedMoC -> Program -> MachineState -> IO ()
runPrintTrace i xmoc@(ExtendedMoC _ moci) program mstate = do
    putStrLn $ case moci >>= printMState of
        Just _ -> let r = registers $ fromJust moci in
            "Program state," ++
            (concat . intersperse "," $ map (\i -> 'R' : show i) [1..r])
            ++ ",Predicate sequence"
        Nothing -> "Program state,Machine state,Predicate sequence"
    runPrintTrace' i xmoc program "Start" mstate


runPrintTrace' :: Maybe Int -> ExtendedMoC -> Program -> ProgramState -> MachineState -> IO ()
runPrintTrace' i xmoc@(ExtendedMoC _ moci) program pstate mstate =
    if (((> 0) <$> i) == Just False || pstate == "End") -- no steps left or end state reached
        then do
            printCSV moci pstate mstate []
        else do
            let i' = (subtract 1) <$> i
                (pstate', mstate', trace) = eval xmoc program pstate mstate
            printCSV moci pstate mstate trace
            runPrintTrace' i' xmoc program pstate' mstate'


printCSV :: Maybe MoCInfo -> ProgramState -> MachineState -> PredicateSequence -> IO ()
printCSV moci pstate mstate trace =
    putStrLn . remnewline $ pstate ++ "," ++ mstate' ++ "," ++ trace'
    where
        mstate' = case moci >>= printMState of
            Just printer -> printer mstate
            Nothing -> mstate
        trace'  = concat $ intersperse " -> " $ map (\(p, b) -> p ++ " " ++ show b) trace


remnewline :: String -> String
remnewline s = filter (\c -> c /= '\n') s


err = "Error running program: "
