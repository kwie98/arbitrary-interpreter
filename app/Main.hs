module Main where

import ArbitraryInterpreter.Parse.ParseMoC
import ArbitraryInterpreter.Parse.ParseCollection
import ArbitraryInterpreter.Exec.RunProgram
import Data.Map.Strict (elems)
import Data.List (sort)
import Control.Monad
import System.IO
import Text.Read

main = do
    putStrLn "Enter program file location: "
    path <- getLine
    -- let path = "C:\\Users\\Konrad\\Documents\\Uni\\arbitrary-interpreter\\test\\program1.txt"
    putStrLn "Enter input machine state: "
    mstate <- getLine
    foo path mstate


foo :: String -> String -> IO ()
foo path mstate = forever $ do
    putStrLn "Enter number of steps to compute: "
    steps <- fmap (readMaybe) getLine :: IO (Maybe Int)
    progText <- readFile path
    let (moc, progs) = parseCollection progText
        res = runSafe steps moc (head . sort $ elems progs) mstate
    putStrLn $ fst res ++ " " ++ snd res


-- bar :: String -> String -> IO ()
-- bar path mstate = do
--     prog <- readFile path
--     print $ evalSafe (parseMoC prog) (parseProgram prog) "Start" mstate

t = "    R1L\n\
    \        R1A\n\
    \            R1B\n\
    \                R1C\n\
    \                    UN\n\
    \                    Z2C\n\
    \                Z2B\n\
    \            Z2A\n\
    \        End\n"

t2 = "    Z3\n"

program2 = "#MOC CM 2\n\
           \#PROGRAM a\n\
           \Start:\n\
           \  Z1\n\
           \Z1 / NOP:\n\
           \  End\n"

program3 = "#sm, 2 registers"
