module ArbitraryInterpreter.Parse.ReadProgramUtil (
prepareProgramText
) where

import Data.List
import Data.Char

-- split program into non-empty lines with no trailing whitespace
-- state names can only consist of alphanumerics
-- predicates and operations can additionally include special characters such as '+', '-', '*', '/', etc.
prepareProgramText :: String -> [String]
prepareProgramText text = filter (not . null) $ map (dropWhileEnd isSpace) $ lines text
