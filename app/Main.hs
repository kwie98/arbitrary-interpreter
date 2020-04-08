{-# LANGUAGE QuasiQuotes #-}
import ArbitraryInterpreter.Exec.RunProgram
import ArbitraryInterpreter.Parse.ParseCollection
import ArbitraryInterpreter.Defs
import Control.Monad (when)
import Data.Map ((!))
import Data.Maybe (fromJust)
import System.Console.Docopt
import System.Environment (getArgs)

-- TODO LICENSE!!!

patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

getArgOrExit = getArgOrExitWith patterns

main = do
    print =<< getArgs
    args <- parseArgsOrExit patterns =<< getArgs

    progText  <- readFile =<< args `getArgOrExit` (argument "FILE")
    progName  <- args `getArgOrExit` (argument "PROGNAME")
    progInput <- getProgInput args

    let (moc, progs) = parseCollection progText
    if (args `isPresent` (shortOption 't'))
        then runPrintTrace Nothing moc (progs ! progName) progInput
        else do
            let (pstate, mstate) = run Nothing moc (progs ! progName) progInput
            putStrLn $ pstate ++ " " ++ mstate


getProgInput :: Arguments -> IO MachineState
getProgInput args
    | args `isPresent` (shortOption 'i') = args `getArgOrExit` (shortOption 'i')
    | args `isPresent` (shortOption 'l') = readFile =<< args `getArgOrExit` (shortOption 'l')
    | otherwise = exitWithUsage patterns
