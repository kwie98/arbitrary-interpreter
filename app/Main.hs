{-# LANGUAGE QuasiQuotes #-}
import ArbitraryInterpreter.Exec.RunProgram
import ArbitraryInterpreter.Parse.ParseCollection
import ArbitraryInterpreter.Defs
import Control.Monad (when)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isNothing)
import System.Console.Docopt
import System.Environment (getArgs)

-- TODO LICENSE!!!
-- TODO test reading input from file

patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

getArgOrExit = getArgOrExitWith patterns

main = do
    args <- parseArgsOrExit patterns =<< getArgs

    progText        <- readFile =<< args `getArgOrExit` (argument "FILE")
    progInput       <- getProgInput args
    let (moc, progs) = parseCollection progText
    progName        <- getProgName args progs

    if (args `isPresent` (shortOption 't'))
        then runPrintTrace Nothing moc (progs Map.! progName) progInput
        else do
            let (pstate, mstate) = run Nothing moc (progs Map.! progName) progInput
            putStrLn $ pstate ++ " " ++ mstate


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
    | isNothing progName = error "bla" -- exitWithUsage patterns
    | Map.member progName' progs = return progName'
    | otherwise = error $ progName' ++ " is not in program collection"
    where
        progName  = args `getArg` (shortOption 'p')
        progName' = fromJust progName
