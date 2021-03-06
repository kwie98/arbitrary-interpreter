module ArbitraryInterpreter.Parse.ParseProgram
( parseProgram
) where

import ArbitraryInterpreter.Defs
import ArbitraryInterpreter.Util.BDTVector
import Data.List
import Data.Char
import qualified Data.HashMap.Strict as Map

-- RELIES ON CLEANED UP PROGRAM TEXT (removed trailing whitespace, comments,
-- empty lines, no MoC definitions and program definitions)
parseProgram :: [String] -> Program
parseProgram plines = buildProgram . collapseProgram $ collapseTrees plines
-- ignore empty lines DONE
-- first line with leading # defines MoC DONE
-- #%moc, %i registers (maybe other auxiliary info?) DONE
-- line with : is state, BDT comes after it DONE
--   state name / operation: (state name and operation cannot include '/') DONE?
-- states cannot appear more than once! DONE


-- Given the lines of the program, concatenate each BDT's lines.
-- Resulting list of strings has elements that either represent a state definition or a whole BDT.
collapseTrees :: [String] -> [String]
collapseTrees []  = []
collapseTrees plines@(x:xs)
    | isStateDef x = x : collapseTrees xs
    | otherwise    = (collapse tree) : collapseTrees rest
    where
        tree          = takeWhile f plines
        rest          = dropWhile f plines
        f             = not . isStateDef


-- collapse multiple lines representing a tree into one string, remove trailing
-- newline and convert indenting to tab characters for parsing. Also remove
-- indenting for root-level nodes. The first line dictates how deep every
-- indent is.
collapse :: [String] -> String
collapse [] = []
collapse plines@(pline@(c:_):_) -- c is first character of first line
    | c == '\t' = init . unlines $ map removeFirstTab plines
    | c == ' '  = init . unlines $ map (removeFirstTab . convertSpaces num) plines
    | otherwise = error $ err ++ "Root node of tree is not indented: " ++ unlines plines
    where
        num = length $ takeWhile (== ' ') pline

        convertSpaces :: Int -> String -> String
        convertSpaces _ ""  = error $ err ++ "Missing indent"
        convertSpaces n str
            | numSpaces `mod` n /= 0 = error $ err ++ "Uneven indent"
            | otherwise              = replicate (numSpaces `div` n) '\t' ++ drop numSpaces str
            where
                numSpaces = length $ takeWhile (== ' ') str

        removeFirstTab :: String -> String
        removeFirstTab "" = error $ err ++ "Missing indent"
        removeFirstTab (c:cs)
            | c == '\t' = cs
            | otherwise = error $ err ++ "Missing indent"
        err = "Error parsing binary decision tree: "


-- check that there are BDTs for every state definition after using collapseTrees
-- also remove whitespace from state definitions
collapseProgram :: [String] -> [(String, String)]
collapseProgram [] = []
collapseProgram [pline]
    | isStateDef pline = error $ err ++ "Missing decision tree for state definition " ++ pline
    | otherwise        = error $ err ++ "Missing state definition for decision tree " ++ pline
    where
        err = "Error parsing program body: "
collapseProgram (p1:p2:plines) -- always needs to be a pair of a state definition and a BDT
    | not $ isStateDef p1 = error $ err ++ "Expected state definition, but found: " ++ p1
    | isStateDef p2       = error $ err ++ "Expected decision tree, but found: " ++ p2
    | otherwise           = (p1, p2) : collapseProgram plines
    where
        err = "Error parsing program body: "


-- put state names and corresponding BDTs into a map
-- RELIES ON VALIDATED PROGRAM STRUCTURE
buildProgram :: [(String, String)] -> Map.HashMap String (String, BDTVector)
buildProgram [] = Map.empty
buildProgram ((def, tree) : plines) =
    Map.unionWithKey dupErr single (buildProgram plines)
    where
        dupErr k _ _ = error $ err ++ "Duplicate state definition for state " ++ k
        single       = Map.singleton stateName (operation, bdtVector)
        stateName    = getState def
        operation    = getOp def
        bdtVector    = treeToVector $ read tree
        err          = "Error parsing program body: "


-- checks if a line of the program is a state definition
-- ProgramState / OpName or PState / $ProgName [n...]:
isStateDef :: String -> Bool
isStateDef "" = False
isStateDef line
    | isStartDef line        = True
    | getState line == "End" = False -- states cannot be named "End"
    | otherwise              = last line' == ':' && elem '/' line' && nameExists && opExists
    where
        nameExists = not . null $ getState line
        opExists   = not . null $ getOp line
        line'      = filter (not . isSpace) line


-- checks if a line of the program is the state definition for the start state
isStartDef :: String -> Bool
isStartDef line = filter (not . isSpace) line == "Start:"


-- from a state definition, extracts the state name
getState :: String -> String
getState def
    | isStartDef def = "Start"
    | otherwise      = trim $ takeWhile (/= '/') def


-- from a state definition, extracts the operation. Trims before and after
-- removing column at the end, and StateName and slash at the start
getOp :: String -> String
getOp def
    | isStartDef def = "NOP"
    | otherwise      = trim . init . tail . dropWhile (/= '/') $ trim def
    where
        def' = filter (not . isSpace) def


-- trim leading and trailing whitespace
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
