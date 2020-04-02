module ArbitraryInterpreter.Parse.ParseCollection
( parseCollection
, trimProgramText
) where

import ArbitraryInterpreter.Defs
import ArbitraryInterpreter.Parse.ParseMoC
import ArbitraryInterpreter.Parse.ParseProgram
import Data.List (dropWhileEnd)
import Data.List.Split (splitOn)
import Data.Char (isSpace)
import qualified Data.HashMap.Strict as Map

-- Parses multiple programs in one file, all using the same MoC
-- Expanded MoC includes name of each program as an operation
-- TODO empty program?
parseCollection :: String -> (MoC, Map.HashMap ProgramName Program)
parseCollection text = (expandedMoC, programs)
    where
        trimmedText = trimProgramText text
        definedMoC = parseMoC $ head trimmedText
        expandedMoC = definedMoC -- TODO add program names

        programs = collapsePrograms $ tail trimmedText


-- makes pairs of program definitions and their respective lines
collapsePrograms :: [String] -> Map.HashMap ProgramName Program
collapsePrograms [] = Map.empty
collapsePrograms (fl1:flines) = case isProgramDef fl1 of
    True  -> Map.unionWithKey dupErr single (collapsePrograms $ dropWhile f flines)
    False -> error $ err ++ "Expected program definition, got: " ++ fl1
    where
        dupErr k _ _ = error $ err ++ "Duplicate program definition for program " ++ k
        single       = Map.singleton (getProgramName fl1) (parseProgram $ takeWhile f flines)
        f            = not . isProgramDef
        err = "Error parsing program collection: "


-- Returns lines of program, removing empty lines, trailing whitespace
-- and // line comments
trimProgramText :: String -> [String]
trimProgramText text = filter (not . null) $ map (dropCmtsWspace) $ lines text
    where
        dropCmtsWspace line = dropWhileEnd isSpace (head $ splitOn "//" line)


-- checks if a line of the program is a program definition
-- #PROGRAM ProgramName
isProgramDef :: String -> Bool
isProgramDef "" = False
isProgramDef line =
    length els == 2 &&
    head   els == "#PROGRAM"
    -- && all (isAlphaNum) els !! 1
    where
        els = words line


-- from a program definition, extracts the program name
getProgramName :: String -> ProgramName
getProgramName def = last $ words def


program2 = "#MOC CM 2\n\n\
           \#PROGRAM a\n\
           \Start:\n\n\
           \  Z1\n\
           \Z1 / NOP://comment\n\
           \  End\n"
