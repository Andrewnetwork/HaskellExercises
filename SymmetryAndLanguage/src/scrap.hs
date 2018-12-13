
adjMatrixPairsMerge :: [(Int,Int)] -> [Int]
adjMatrixPairsMerge pairList = adjMatrixPairsMerge' $ map tupleToList pairList

adjMatrixPairsMerge' :: [[Int]] -> [Int]
adjMatrixPairsMerge' (x:[]) = x
adjMatrixPairsMerge' (x:xs)
  | isJust popElm = adjMatrixPairsMerge' $ newHead++popedList
  | otherwise = adjMatrixPairsMerge' (xs++[x])
  where (popElm,popedList) = selectivePop xs (\(z:zs)->z==(last x))
        newHead = [(concatOnHead x (fromJust popElm))]


canMerge :: Eq a => [a] -> [a] -> Bool
canMerge left right = (last left) == (head right)

otherElements particle world = filter (\scan->particle /= scan) world

makeBranch :: [Int] -> [[Int]] -> MergeTree
makeBranch root candidates = MergeTree (Node root) (buildMergeTree' root (otherElements root candidates))

buildMergeTree' :: [Int] -> [[Int]] -> [MergeTree]
buildMergeTree' root candidates
  | length successfulCandidates > 0 = map (\candidate -> makeBranch candidate successfulCandidates) candidates
  | otherwise = [NullElm]
  where successfulCandidates = filter (\candidate->canMerge root candidate) candidates
