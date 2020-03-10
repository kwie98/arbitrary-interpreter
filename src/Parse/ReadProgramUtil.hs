module Parse.ReadProgramUtil (
prepareProgramText
) where

import Data.List
import Data.Char

-- split program into non-empty lines with no trailing whitespace
prepareProgramText :: String -> [String]
prepareProgramText text = filter (not . null) $ map (dropWhileEnd isSpace) $ lines text
