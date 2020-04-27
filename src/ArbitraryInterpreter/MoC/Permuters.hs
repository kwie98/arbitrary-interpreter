module ArbitraryInterpreter.MoC.Permuters where

import ArbitraryInterpreter.Defs
import Data.List (sortOn)

-- given a permutation and a list, the inverse of the permutation is applied to
-- the list
invPermute :: [Int] -> [a] -> [a]
invPermute [] xs = xs
invPermute p  xs = map snd $ sortOn fst $ zip p xs


permuteStrings :: [Int] -> MachineState -> MachineState
permuteStrings [] ms = ms
permuteStrings p ms = show $ invPermute p (read ms :: [String])


permuteInts :: [Int] -> MachineState -> MachineState
permuteInts [] ms = ms
permuteInts p ms = show $ invPermute p (read ms :: [Int])
