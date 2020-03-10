module BDTVector
( BDTVector
, treeToVector
) where

import qualified Data.Vector as Vector
import qualified LabeledTree as LT

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
