module Parse.ParseProgram (
Program,
parseProgram
) where

import MoC.MoC
import MoC.CounterMachine
import MoC.StackMachine
import Util.BDTVector
import Parse.ReadProgramUtil
import Data.List
import Data.Char
import qualified Data.HashMap.Strict as Map

type Program = Map.HashMap String (String, BDTVector)

parseProgram :: String -> Program
parseProgram text = buildMap body
    where
        plines  = prepareProgramText text
        body    = validateStructure . collapseTrees $ tail plines
-- ignore empty lines DONE
-- first line with leading # defines MoC DONE
-- #%moc, %i registers (maybe other auxiliary info?) DONE
-- line with : is state, BDT comes after it DONE
--   state name / operation: (state name and operation cannot include '/') DONE?
-- states cannot appear more than once! DONE


-- check that there are BDTs for every state definition after using collapseTrees
-- also remove whitespace from state definitions
validateStructure :: [String] -> [String]
validateStructure [] = []
validateStructure [pline]
    | isStateDef pline = error $ err ++ "Missing decision tree for state definition " ++ pline
    | otherwise        = error $ err ++ "Missing state definition for decision tree " ++ pline
    where
        err = "Error parsing program body: "
validateStructure (p1:p2:plines) -- always needs to be a pair of a state definition and a BDT
    | not $ isStateDef p1 = error $ err ++ "Expected state definition, but found: " ++ p1
    | isStateDef p2       = error $ err ++ "Expected decision tree, but found: " ++ p2
    | otherwise           = (filter (not . isSpace) p1) : p2 : validateStructure plines
    where
        err = "Error parsing program body: "


-- put state names and corresponding BDTs into a map
-- RELIES ON VALIDATED PROGRAM STRUCTURE
buildMap :: [String] -> Map.HashMap String (String, BDTVector)
buildMap [] = Map.empty
buildMap (def:tree:plines) = Map.unionWithKey throwError single (buildMap plines)
    where
        throwError k _ _ = error $ err ++ "Duplicate state definition for state " ++ k
        single           = Map.singleton stateName (operation, bdtVector)
        stateName        = getState def
        operation        = getOp def
        bdtVector        = treeToVector $ read tree
        err              = "Error parsing program body: "


-- check if a line of the program is a state definition
isStateDef :: String -> Bool
isStateDef ""   = False
isStateDef line
    | isStartDef line        = True
    | getState line == "End" = False -- states cannot be named "End"
    | otherwise              = last line' == ':' && elem '/' line' && nameExists && opExists
    where
        nameExists = not . null $ getState line
        opExists   = not . null $ getOp line
        line'      = filter (not . isSpace) line


-- check if a line of the program is the state definition for the start state
isStartDef :: String -> Bool
isStartDef line = filter (not . isSpace) line == "Start:"


-- from a state definition, extract the state name
getState :: String -> String
getState def = takeWhile (/= '/') def'
    where def' = filter (not . isSpace) def

-- from a state definition, extract the operation
getOp :: String -> String
getOp def
    | isStartDef def = "NOP"
    | otherwise      = init . tail $ dropWhile (/= '/') def'
    where
        def' = filter (not . isSpace) def


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
collapse plines@(pline@(c:_):_) -- first character of first line
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
