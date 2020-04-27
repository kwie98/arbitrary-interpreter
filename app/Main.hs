{-# LANGUAGE QuasiQuotes #-}
import ArbitraryInterpreter.Exec.RunProgram
import ArbitraryInterpreter.Parse.ParseCollection
import ArbitraryInterpreter.Defs
import Control.Monad (when)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isNothing)
import System.Console.Docopt
import System.Environment (getArgs)
import Text.Read (readMaybe)

-- TODO LICENSE!!!

patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

getArgOrExit = getArgOrExitWith patterns

main = do
    a <- getArgs
    let args = case parseArgs patterns a of
            Left err -> error $ show err ++ "\n" ++ usage patterns
            Right res -> res
    -- print args

    progText         <- readFile =<< args `getArgOrExit` (argument "FILE")
    progInput        <- getProgInput args
    let (xmoc, progs) = parseCollection progText
        maxSteps      = readMaybe =<< args `getArg` (shortOption 's')
    progName         <- getProgName args progs

    if (args `isPresent` (shortOption 't'))
        then do
            runPrintTrace maxSteps xmoc (progs Map.! progName) progInput
        else do
            let (pstate, mstate) = run maxSteps xmoc (progs Map.! progName) progInput
            putStrLn . remnewline $ pstate ++ " " ++ mstate


getProgInput :: Arguments -> IO MachineState
getProgInput args
    | args `isPresent` (shortOption 'i') = args `getArgOrExit` (shortOption 'i')
    | args `isPresent` (shortOption 'l') = readFile =<< args `getArgOrExit` (shortOption 'l')
    | otherwise = exitWithUsage patterns


-- in case the program collection only contains one program, the program name
-- can be omitted. Should this not be the case and the program name is still
-- omitted, or should the program name not appear in the program map, an error
-- is thrown.
getProgName :: Arguments -> Map.Map ProgramName Program -> IO ProgramName
getProgName args progs
    | Map.size progs == 1 && isNothing progName = return . head $ Map.keys progs
    | isNothing progName = exitWithUsage patterns
    | Map.member progName' progs = return progName'
    | otherwise = error $ progName' ++ " is not in program collection"
    where
        progName  = args `getArg` (shortOption 'p')
        progName' = fromJust progName


remnewline :: String -> String
remnewline s = filter (\c -> c /= '\n') s
