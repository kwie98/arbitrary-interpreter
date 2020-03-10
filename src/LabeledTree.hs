module LabeledTree where

import SPLib

data LabeledTree = LabeledTree String [LabeledTree] -- (Eq,Show,Read)
type Path = [Int] -- [] = root node, [0,2] = first child of root node, then third child of that node

rootDegree :: LabeledTree -> Int
rootLabel :: LabeledTree -> String

getLabel :: LabeledTree -> Path -> String
setLabel :: LabeledTree -> Path -> String -> LabeledTree

subtree :: LabeledTree -> Path -> LabeledTree
pathExists :: LabeledTree -> Path -> Bool

appendSubtree :: LabeledTree -> Path -> LabeledTree -> LabeledTree
removeSubtree :: LabeledTree -> Path -> Int -> LabeledTree
insertSubtree :: LabeledTree -> Path -> Int -> LabeledTree -> LabeledTree
setSubtree    :: LabeledTree -> Path -> Int -> LabeledTree -> LabeledTree

findLabelPath :: LabeledTree -> [String] -> (Maybe Path)
findPathByKey :: LabeledTree -> [String] -> Int -> [String] -> (Maybe Path)


-- DEFINITIONS

instance Eq LabeledTree where
  (LabeledTree l1 c1) == (LabeledTree l2 c2) =
    (if' ((l1 == l2) && ((length c1) == (length c2)))
      (if' ((length c1) == 0) True (reduce (&&) x))
      False
    )
    where
    x = (map (\(x,y) -> (x==y)) (zip c1 c2))

instance Show LabeledTree where
  show tree = (showLabeledTree tree 0)

showLabeledTree :: LabeledTree -> Int -> String
showLabeledTree (LabeledTree label children) depth = (join "\n" res)
    where
      rootStr = (take depth (repeat '\t')) ++ (escape label)
      childrenStr = (map (\x -> (showLabeledTree x (depth+1))) children)
      res = (prepend childrenStr rootStr)

instance Read LabeledTree where
  readsPrec _ = readLabeledTree

readLabeledTree :: String -> [(LabeledTree, String)]
readLabeledTree str =
  (if' ((fst (first lines))>0) (error "root node has leading tab(s)")
    (if' ((length lines) == 1)
      [(tree,"")]
      [((readLabeledTree_h tree [] (removeFirst lines)),"")]
    )
  )
  where
    tree = (LabeledTree (unescape (snd (first lines))) [])
    lines = (map f (split "\n" str))
    f line = (i,(drop i line)) -- "\t\tabc" -> (2,"abc")
      where
        i = (just (find pred (reverse [0..(length line)])))
        pred i = (((take i (repeat '\t'))++(drop i line))==line)

readLabeledTree_h tree curNode lines = --(trace (show (d,l,curNode,tree))  --(trace (show tree)  --(trace (show (line,d,l,aNode))
    (if' (d==0) (error "non-root node has 0 leading tabs")
      (if' ((length lines)>1)
        (readLabeledTree_h (appendSubtree tree aNode (LabeledTree l [])) (append aNode k) (removeFirst lines))
        (appendSubtree tree aNode (LabeledTree l []))
      )
    )
    --)
    where
      line = (first lines)
      d = (fst line) --depth of next node
      l = (unescape (snd line)) --label of next node
      aNode = (reverse (drop ((length curNode) - d + 1) (reverse curNode)))
      k = (rootDegree (subtree tree aNode)) --next node is (k+1)-th child of curNode


rootDegree (LabeledTree _ c) = (length c)
rootLabel (LabeledTree l _) = l

subtree (LabeledTree l c) p = (if' (isEmpty p) (LabeledTree l c) (subtree (get c (first p)) (removeFirst p)) )
getLabel (LabeledTree l c) p = (if' (isEmpty p) l (getLabel (get c (first p)) (removeFirst p)) )
pathExists (LabeledTree l c) p =
  (if' (isEmpty p)
    True
    (if' (((first p) >= 0) && ((first p) < (length c)))
      (pathExists (get c (first p)) (removeFirst p))
      False
    )
  )

genericModifyTree  :: (LabeledTree -> LabeledTree) -> LabeledTree -> Path -> LabeledTree
genericModifyTree f (LabeledTree l c) p =
  (if' (isEmpty p)
      (f (LabeledTree l c))
      (LabeledTree l c')
  )
  where
    c' = (set c (first p) (genericModifyTree f (get c (first p)) (removeFirst p)))


setLabel tree path label = (genericModifyTree f tree path)
  where
    f (LabeledTree _ c) = (LabeledTree label c)

appendSubtree tree path subtree = (genericModifyTree f tree path)
  where
    f (LabeledTree l c) = (LabeledTree l (append c subtree))

removeSubtree tree path i = (genericModifyTree f tree path)
  where
    f (LabeledTree l c) = (LabeledTree l (remove c i))

insertSubtree tree path i subtree = (genericModifyTree f tree path)
  where
    f (LabeledTree l c) = (LabeledTree l (insert c i subtree))

setSubtree tree path i subtree = (genericModifyTree f tree path)
  where
    f (LabeledTree l c) = (LabeledTree l (set c i subtree))

--ex: (a (b c) (b d) (b x e)) [a,b,e] -> [2,1]
findLabelPath tree labelPath = (findLabelPath_h tree labelPath 0)

findLabelPath_h (LabeledTree l c) lp offset =
  (if' (isEmpty lp)
    (Just [])
    (if' (contains cl (first lp))
      (if' (isNothing x)
        (findLabelPath_h (LabeledTree l (remove c index)) lp (offset+1))
        (Just (prepend (just x) (index+offset)))
      )
      Nothing
    )
  )
  where
    cl = (map (\t -> (rootLabel t)) c)
    index = (indexOf cl (first lp))
    x = (findLabelPath (get c index) (removeFirst lp))

findPathByKey tree labelPath up labelPath2 =
  (if' (isNothing p1M) Nothing
    (if' (isNothing p2M) Nothing
      (Just res)
    )
  )
  where
    p1M = (findLabelPath tree labelPath)
    p1 = (just p1M)
    p1up = (reverse (drop up (reverse p1)))
    p2M = (findLabelPath (subtree tree p1up) labelPath2)
    res = p1up++(just p2M)

escapeTable = [ ('\\',"\\\\"), ('\n',"\\n"), ('\t',"\\t") ] --important: "\\" must come before the others
escape str = (escape_h str escapeTable)
escape_h str [] = str
escape_h str ((x,y):z) = (escape_h (join y (split [x] str)) z)

unescape str =
  (if' (contains str '\\')
    (if' (isEmpty y) (error "string to unescape contains trailing '\\'")
      (if' (not valid) (error "string to unescape contains invalid escape sequence")
        (x++(prepend (unescape (removeFirst y)) realChar))
      )
    )
    str
  )
  where
    (x,y) = (splitAtFirst "\\" str)
    a = ['\\','n','t']
    valid = ((not (isEmpty y)) && (contains a (first y)))
    index = (indexOf a (first y))
    realChar = (fst (get escapeTable index))


-- check if a labeled tree is a binary tree, i.e. if every node has at most two children
isBinaryTree :: LabeledTree -> Bool
isBinaryTree (LabeledTree _ [])         = True
isBinaryTree (LabeledTree _ (fc:[]))    = isBinaryTree fc
isBinaryTree (LabeledTree _ (fc:sc:[])) = isBinaryTree fc && isBinaryTree sc
isBinaryTree _                          = False


-- map binary tree labels to positions in array representation
mapNodes :: LabeledTree -> [(Int, String)]
mapNodes tree =
    if not $ isBinaryTree tree
        then error "mapNodes: Given tree not binary"
        else mapNodes' 0 tree
-- all positions need to be non-negative,
-- lists needs to have exactly as many elements as tree has nodes,
-- no position can occur more than once,
-- labels need to be mapped correctly


-- recursive function for mapNodes, also gets position of current node as parameter
mapNodes' :: Int -> LabeledTree -> [(Int, String)]
mapNodes' pos (LabeledTree label [])         = [(pos, label)]
mapNodes' pos (LabeledTree label (fc:[]))    = (pos, label) : mapNodes' (pos*2+1) fc
mapNodes' pos (LabeledTree label (fc:sc:[])) = (pos, label) : mapNodes' (pos*2+1) fc ++ mapNodes' (pos*2+2) sc
mapNodes' _   (LabeledTree _     (_:_:_))    = error "mapNodes': Given tree not binary"
