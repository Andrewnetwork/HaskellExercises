-- Sequence.hs
-- Andrew Ribeiro
-- December 13, 2018

module Sequence where
import           Data.List
import           Data.Maybe        (fromJust,isJust)
import           ListManipulations
import           Trees
import           Symmetry

data AdjacencyMatrix = AdjacencyMatrix [Int] [[Int]]
data MergeTree = MergeTree MergeTree [MergeTree] | Node [Int] | NullElm deriving Show

gridString :: (Show a) => [[a]] -> String
gridString  []   = "┌─┐\n└─┘\n"
gridString  [[]] = "┌─┐\n└─┘\n"
gridString  xxs  = (++ bot) $ concat $ zipWith (\a b -> unlines [a, b]) (top : replicate rowC mid) rows
  where
    rowC   = pred . length $ xxs
    colC   = pred . length . head $ xxs
    top    = "┌" ++ repC "─┬" ++ "─┐"
    mid    = "├" ++ repC "─┼" ++ "─┤"
    bot    = "└" ++ repC "─┴" ++ "─┘"
    repC  = concat . replicate colC
    rows   = (++ "|") . ('|' :) . intercalate "|" . ((\x -> show x) <$>) <$> xxs

instance Show AdjacencyMatrix where
  show (AdjacencyMatrix labels matrix) = gridString matrix

--AdjacencyMatrix [1,2,3] [[1,2,2],[4,2,1],[4,5,2]]
initAdjacencyMatrix :: [Int] -> AdjacencyMatrix
initAdjacencyMatrix ls = AdjacencyMatrix uniqueElms (replicate uniqueElmsLen (replicate uniqueElmsLen 0))
                         where uniqueElms = (treeNub ls)
                               uniqueElmsLen = (length uniqueElms)

--seqAdjacency [1,2,3] -> AdjacencyMatrix [1,2,3] [[1,0,0],[0,1,0],[0,0,1]]
addAdjacency :: Int -> Int -> AdjacencyMatrix -> AdjacencyMatrix
addAdjacency from to (AdjacencyMatrix labels mat) = AdjacencyMatrix labels (matrixInsert weight (fromLoc,toLoc) mat)
                                                    where fromLoc = fromJust $ elemIndex from labels
                                                          toLoc = fromJust $ elemIndex to labels
                                                          weight = (mat!!fromLoc!!toLoc + 1)
-- addAdjacency 1 2 (initAdjacencyMatrix [1,2,3])

seqAdjacency :: [Int] -> AdjacencyMatrix
seqAdjacency ls = seqAdjacency' (listNeighbors ls) (initAdjacencyMatrix ls)
seqAdjacency' [] adjMat         = adjMat
seqAdjacency' ((a,b):xs) adjMat = seqAdjacency' xs (addAdjacency a b adjMat)


adjMatrixPairs mat@(AdjacencyMatrix labels _ ) = foldr (++) [] $ foldr (++) [] (adjMatrixPairs' mat labels)
adjMatrixPairs' (AdjacencyMatrix _ [] ) labels = []
adjMatrixPairs' (AdjacencyMatrix (y:ys) (x:xs) ) labels = tupleRow : adjMatrixPairs' (AdjacencyMatrix ys xs) labels
                                                          where pairs = zip (repeat y) labels
                                                                tupleRow = map (\(a,b)-> a b ) (zip (map replicate x) pairs)


adjMatrix (AdjacencyMatrix _ matrix) = matrix

-- user: newmaidumosa
candidates :: [a] -> [a] -> [(a, [a])]
candidates acc [] = []
candidates acc (x:xs) = (x, acc ++ xs) : candidates (x : acc) xs
-- candidates [] [1,2,3,4,5,6,7]

-- user: newmaidumosa
combinations :: (Eq a, Show a) => [[a]] -> [[a]]
combinations = nub . (merge =<<) . candidates []

-- user: newmaidumosa
merge :: Eq a => ([a], [[a]]) -> [[a]]
merge (acc, [])   = [acc]
merge (acc, rest) = merge' =<< eachCandidate
  where
    eachCandidate    = candidates [] next
    (next, noMatch)  = partition (\e -> last acc == head e) rest
    merge' (_:y, ys) = merge (acc ++ y, noMatch ++ ys)

sequenceClass ls = combinations $ map tupleToList (adjMatrixPairs.seqAdjacency $ ls)

adjMatrixToSeqClass :: AdjacencyMatrix -> [[Int]]
adjMatrixToSeqClass adjMat = combinations $ map tupleToList (adjMatrixPairs adjMat)
-- sequenceClass [10,11,12,10,11,12] -> [[10,11,12,10,11,12]]
-- sequenceClass [1,1,2,2,3,4,5,6,3,3] -> [[1,1,2,2,3,3,4,5,6,3],[1,1,2,2,3,4,5,6,3,3]]
-- sequenceClass [1,1,2,2,3,0,5,6,3,3] -> [[1,1,2,2,3,0,5,6,3,3],[1,1,2,2,3,3,0,5,6,3]]
-- sequenceClass [1,3,2,1,3,2,2,1,3,2,1,3] -> [[1,3,2,1,3,2,1,3,2,2,1,3],[1,3,2,1,3,2,2,1,3,2,1,3],[1,3,2,2,1,3,2,1,3,2,1,3]]

-- Per each sequence length, what is the distribution of the number of
-- sequences in that classs.

applySym :: ([[Int]] -> [[Int]]) -> [Int] -> AdjacencyMatrix
applySym symmetryFn ls = AdjacencyMatrix labels (symmetryFn $ matrix)
                         where (AdjacencyMatrix labels matrix) = seqAdjacency ls

symmetryCousins :: ([[Int]] -> [[Int]]) -> [Int] -> [[Int]]
symmetryCousins symmetryFn ls = adjMatrixToSeqClass (applySym symmetryFn ls)

adjMatrixSymmetryPath :: AdjacencyMatrix -> AdjacencyMatrix -> [SymmetryPath]
adjMatrixSymmetryPath (AdjacencyMatrix _ source) (AdjacencyMatrix _ dest) = symPath source dest
-- applySym (horz.diag) [10,11,12,10,11,12]
-- symmetryCousins (horz.diag) [10,11,12,10,11,12]
-- sequenceClass [10,11,12,10,11,12]

-- symmetryCousins (diag) [10,11,12,10,11,12]
-- adjMatrixToSeqClass $ applySym (horz.diag) [10,11,12,10,11,12]

seqSymmPath :: [Int] -> [Int] -> [SymmetryPath]
seqSymmPath s1 s2 = adjMatrixSymmetryPath (seqAdjacency s1) (seqAdjacency s2)

adjacencyMatrixTree :: [Int] -> [[Int]]
adjacencyMatrixTree ls = nub $ concatMap adjMatrixToSeqClass adjMatrixList
                         where (AdjacencyMatrix label mat) = (seqAdjacency ls)
                               symmetryTree = makeSymTree mat
                               adjMatrixList = map (AdjacencyMatrix label) (getParents symmetryTree)

-- adjacencyMatrixTree [10,11,12,10,11,12]
-- adjacencyMatrixTree [1,2,3,3,2,1]
-- seqSymmPath [1,2,3,3,2,1] [3,2,1,2,3,1]
-- seqAdjacency [1,2,3,3,2,1]
-- seqAdjacency [3,2,1,2,3,1]


-- allSequenceSym ls =              --symmetryCousins
--                     where (AdjacencyMatrix label mat) = (seqAdjacency ls)  --makeSymTree
--                           symmetryTree = makeSymTree mat


-- allSequenceSym [10,11,12,10,11,12]

-- adjMatrixSymmetryPath (seqAdjacency [10,11,12,10,11,12]) (seqAdjacency [10,11,10,12,11,12])




-- [(10,11),(11,11),(10,11),(11,11),(12,12)]

--  seqAdjacency [10,11,12,10,11,12]
-- --------------- ----
--combinations $ map tupleToList (adjMatrixPairs.seqAdjacency $ [10,11,12,10,11,12])

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
-- [(10,11),(10,11),(11,12),(11,12),(12,10)]
-- buildMergeTree' [1,2] [[3,4],[1,2],[2,4]]
-- buildMergeTree' root candidates
--  map (\(a,b)-> a b ) (zip (map replicate x) pairs)
-- filter (\x-> canMerge [1,2,3] x) [[1,2],[3,4],[4,5]]
--adjMatrixPairs.seqAdjacency $ [10,11,12,10,11,12]
--adjMatrixPairs.seqAdjacency $ [1,1,2,2,3,4,5,6,3,3]
-- zip (repeat 1) [1,2,3]
-- 1 -> 1 , 1 - 2, 1 -> 3
-- zip (repeat labels!!0) (labels)
-- take 4 [1,2,3,4,5]
adjMatrixPairsMerge :: [(Int,Int)] -> [Int]
adjMatrixPairsMerge pairList = adjMatrixPairsMerge' $ map tupleToList pairList

adjMatrixPairsMerge' :: [[Int]] -> [Int]
adjMatrixPairsMerge' (x:[]) = x
adjMatrixPairsMerge' (x:xs)
  | isJust popElm = adjMatrixPairsMerge' $ newHead++popedList
  | otherwise = adjMatrixPairsMerge' (xs++[x])
  where (popElm,popedList) = selectivePop xs (\(z:zs)->z==(last x))
        newHead = [(concatOnHead x (fromJust popElm))]

-- (a,b) = selectivePop [[1,2],[3,4],[5,6]] (\(x:xs)->x==5)
--  traceShow  (show [(10,11),(10,11),(11,12),(11,12),(12,10)]) (adjMatrixPairsMerge [(10,11),(10,11),(11,12),(11,12),(12,10)]
-- adjMatrixPairsMerge [(10,11),(10,11),(11,12),(11,12),(12,10)]
-- [concatOnHead [1,2,3,4,5,6] [2,3]]
--
-- [[1,2,3]] ++ [[4,5,6],[7,8,9]]
-- (adjMatrixToSeq.seqAdjacency $ [1,3,2,1,3,2,2,1,3,2,1,3]) == [1,3,2,1,3,2,2,1,3,2,1,3]
-- seqAdjacency [1,3,2,1,3,2,2,1,3,2,1,3]
-- [(1,3),(1,3),(1,3),(1,3),(2,1),(2,1),(2,1),(2,2),(3,2),(3,2),(3,2)]
-- [(1,3,2),(1,3),(1,3),(1,3),(2,1),(2,1),(2,1),(2,2),(3,2),(3,2)]
-- [(1,3,2,1),(1,3),(1,3),(1,3),(2,1),(2,1),(2,2),(3,2),(3,2)]
-- [(1,3,2,1,3),(1,3),(1,3),(2,1),(2,1),(2,2),(3,2),(3,2)]
-- [(1,3,2,1,3,2),(1,3),(1,3),(2,1),(2,1),(2,2),(3,2)]
-- [(1,3,2,1,3,2,1),(1,3),(1,3),(2,1),(2,2),(3,2)]
-- [(1,3,2,1,3,2,1,3),(1,3),(2,1),(2,2),(3,2)]
-- [(1,3,2,1,3,2,1,3,2),(1,3),(2,1),(2,2)]
-- [(1,3,2,1,3,2,1,3,2,1),(1,3),(2,2)]
-- [(1,3,2,1,3,2,1,3,2,1,3),(2,2)]

-- Problem given a list of tuples, return all the ways the tuples can be merged.
-- Make a tree of all the possible combinations.
-- Merging procedure: A sublist can be merged if their first and last elements
-- match.
