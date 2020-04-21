module ArbitraryInterpreter.Parse.ParseCollection
( parseCollection
, trimProgramText
, isProgramDef
) where

import ArbitraryInterpreter.Defs
import ArbitraryInterpreter.Parse.ParseMoC
import ArbitraryInterpreter.Parse.ParseProgram
import ArbitraryInterpreter.Parse.PreExecCheck
import ArbitraryInterpreter.Exec.RunProgram
import Data.List (dropWhileEnd, foldl')
import Data.List.Split (splitOn)
import Data.Char (isSpace)
import qualified Data.Map.Strict as Map

-- Parses multiple programs in one file, all using the same MoC
-- Expanded MoC includes name of each program as an operation
-- Programs are saved in a normal map (as opposed to BDTs, which are saved in
-- HashMaps) since ordering of programs does matter here
-- TODO maybe change this back
parseCollection :: String -> (MoC, Map.Map ProgramName Program)
parseCollection ""   = error $ err ++ "Empty file"
parseCollection text = (expandedMoC, programMap)
    where
        trimmedText = trimProgramText text
        programs = collapsePrograms $ tail trimmedText
        programMap = Map.fromListWithKey dupErr programs
        dupErr k _ _ = error $ err ++ "Duplicate program definition for program " ++ k

        definedMoC = parseMoC $ head trimmedText
        expandedMoC = foldl'
            (\moc (pname, p) -> case preExecCheck moc p of
                False -> error $ err ++ "Invalid sub-program: " ++ pname
                True  -> addOperation moc ('$':pname) (\mstate -> snd $ run Nothing moc p mstate)
            )
            definedMoC programs


-- makes pairs of program definitions and their respective lines
collapsePrograms :: [String] -> [(ProgramName, Program)]
collapsePrograms [] = []
collapsePrograms (fl1:flines) = case isProgramDef fl1 of
    True  -> single : (collapsePrograms $ dropWhile f flines)
    False -> error $ err ++ "Expected program definition, got: " ++ fl1
    where
        single       = (getProgramName fl1, parseProgram $ takeWhile f flines)
        f            = not . isProgramDef


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

err = "Error parsing program collection: "
