module Util.BDTVector (
BDTVector,
treeToVector
) where

import Data.Maybe
import qualified Data.Vector as Vector
import qualified Util.LabeledTree as LT

type BDTVector = Vector.Vector String

-- Put the Data from a labeled binary (decision) tree into a Vector.
treeToVector :: LT.LabeledTree -> BDTVector
treeToVector tree =
    if not $ LT.isBinaryTree tree
    then error "treeToArray: Given tree not binary"
    else Vector.replicate len "" Vector.// mappedNodes
        where
            mappedNodes = LT.mapNodes tree
            len = (maximum $ map (fst) mappedNodes) + 1
-- no trailing empty strings,
-- all labels from tree present and in correct order (breadth first)


isNode :: BDTVector -> Int -> Bool
isNode v i
    | isNothing val       = False
    | null $ fromJust val = False
    | otherwise           = True
    where
        val = v Vector.!? i


isLeaf :: BDTVector -> Int -> Bool
isLeaf v i
    | not $ isNode v i       = False
    | not $ isNode v (i * 2) = False -- position of first child
    | otherwise              = True


isInner :: BDTVector -> Int -> Bool
isInner v i = (isNode v i) && (not $ isLeaf v i)
