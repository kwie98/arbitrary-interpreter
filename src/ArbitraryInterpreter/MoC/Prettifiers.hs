module ArbitraryInterpreter.MoC.Prettifiers where

import ArbitraryInterpreter.Defs
import Data.List (intersperse)


prettify :: Show a => [a] -> String
prettify xs = concat . intersperse "," $ map (show) xs


prettifyStrings :: MachineState -> String
prettifyStrings ms = prettify (read ms :: [String])


prettifyInts :: MachineState -> String
prettifyInts ms = prettify (read ms :: [Int])
