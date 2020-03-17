module ArbitraryInterpreter.Util.BDTVector
( treeToVector
, isNode
, isLeaf
, isBranch
, leaves
, branches
) where

import ArbitraryInterpreter.Defs
import ArbitraryInterpreter.Util.LabeledTree (LabeledTree, mapNodes, isBinaryTree)
import Data.Maybe
import qualified Data.Vector as Vector

-- Put the Data from a labeled binary (decision) tree into a Vector.
treeToVector :: LabeledTree -> BDTVector
treeToVector tree =
    if not $ isBinaryTree tree
    then error "treeToArray: Given tree not binary"
    else Vector.replicate len "" Vector.// mappedNodes
        where
            mappedNodes = mapNodes tree
            len = (maximum $ map (fst) mappedNodes) + 1
-- no trailing empty strings,
-- all labels from tree present and in correct order (breadth first)


isNode :: BDTVector -> Int -> Bool
isNode bdt i
    | isNothing val       = False
    | null $ fromJust val = False
    | otherwise           = True
    where
        val = bdt Vector.!? i


isLeaf :: BDTVector -> Int -> Bool
isLeaf bdt i
    | not $ isNode bdt i     = False
    | isNode bdt (i * 2 + 1) = False -- position of first child
    | otherwise              = True


isBranch :: BDTVector -> Int -> Bool
isBranch bdt i = (isNode bdt i) && (not $ isLeaf bdt i)


leaves :: BDTVector -> [String]
leaves bdt = leaves' bdt 0


leaves' :: BDTVector -> Int -> [String]
leaves' bdt i
    | isLeaf bdt i = [bdt Vector.! i]
    | isNode bdt i = leaves' bdt (i * 2 + 1) ++ leaves' bdt (i * 2 + 2)
    | otherwise    = []


branches :: BDTVector -> [String]
branches bdt = branches' bdt 0


branches' :: BDTVector -> Int -> [String]
branches' bdt i
    | isBranch bdt i = (bdt Vector.! i) : branches' bdt (i * 2 + 1) ++ branches' bdt (i * 2 + 2)
    | otherwise      = []
