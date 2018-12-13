
module Graphs where
import           Data.List         (elemIndex)
import           Data.Maybe        (fromJust)
import           IO
import           ListManipulations
import           Numbers
import           Trees
import           Data.Maybe
import           Debug.Trace

data AdjacencyMatrix = AdjacencyMatrix [Int] [[Int]]
data MergeTree = MergeTree MergeTree [MergeTree] | Node [Int] | NullElm deriving Show

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

-- sequence -> listNeighbors -> adjMat
-- [(1,2),(2,3),(3,4),(4,5),(5,6)] -> [(1,2),(5,6),(2,3),(3,4),(4,5)]
-- seqAdjacency [1..20]
-- seqAdjacency [1,2,3,1,2,3,1,2,3,1,2,3]
-- seqAdjacency [1,3,2,1,3,2,2,1,3,2,1,3]
-- seqAdjacency [3,2,1,3,2,1,3,2,1]
-- seqAdjacency [10,11,12,10,11,12]
-- seqAdjacency [11,10,12,10,11,12]
-- seqAdjacency [1,2,2,2,1,1,1,4,4,6]
-- addAdjacency 1 2 (initAdjacencyMatrix [1,2,3])
-- seqAdjacency.collatzSeq $ 30000
-- matrixInsert 3 (1,1) [[0,0,0],[0,0,0],[0,0,0]]
-- [1,2,3]
-- [2,3,4]
-- seqAdjacency $ poolSequence (collatzSeq 100000) 1000
-- TODO: AdjacencyMatrix to sequence.
-- take 4 $ repeat [1,2]
-- replaceElement 3 [3,2] $ take 4 $ repeat [1,2]
-- concat $ take 4 $ repeat [1,2]
-- concatOnHead (concat $ take 4 $ repeat [1,2]) [2,3]
-- sequence -> adjMat
-- adjMatrix -> sequence
--adjMatrixToSeq :: AdjacencyMatrix -> [Int]
--
--adjMatrixPairs (AdjacencyMatrix labels mat) = (head labels)adjMatrixPairs
-- adjMatrixPairs.seqAdjacency $ [10,11,12,10,11,12] -> [(10,11),(10,11),(11,12),(11,12),(12,10)]
--  [(10,11),(10,11),(11,12),(11,12),(12,10)]
--  [[10,11],[10,11],[11,12],[11,12],[12,10]]
--  [[10,11,12],[10,11],[11,12],[12,10]]
--  [[10,11,12,10],[10,11],[11,12]]
--  [[10,11,12,10,11],[11,12]]
--  [[10,11,12,10,11,12]]
--tmpFn :: [(Int,Int)] -> [Int]
--tmpFn ls = tmpFn' $ map tupleToList ls
--tmpFn' (x:xs) = x
--concatOnHead
-- seqAdjacency [2,3,4,1,2,3,4,1]
-- map tupleToList [(1,2),(2,3),(2,3),(3,4),(3,4),(4,1),(4,1)]
-- [[1,2],[2,3],[2,3],[3,4],[3,4],[4,1],[4,1]]
-- [[1,2,3],[2,3],[3,4],[3,4],[4,1],[4,1]]
-- [[1,2,3,4],[2,3],,[3,4],[4,1],[4,1]]
-- [[1,2,3,4,1],[2,3],[3,4],[4,1]] Cannot MERGE!
-- [[2,3],[3,4],[4,1],[1,2,3,4,1]] RR SHUFFLE
-- [[2,3,4],[4,1],[1,2,3,4,1]]
-- [[2,3,4,1],[1,2,3,4,1]]
-- [[2,3,4,1,2,3,4,1]]
--adjMatrixPairs :: AdjacencyMatrix -> [(Int,Int)]
adjMatrixPairs mat@(AdjacencyMatrix labels _ ) = foldr (++) [] $ foldr (++) [] (adjMatrixPairs' mat labels)
adjMatrixPairs' (AdjacencyMatrix _ [] ) labels = []
adjMatrixPairs' (AdjacencyMatrix (y:ys) (x:xs) ) labels = tupleRow : adjMatrixPairs' (AdjacencyMatrix ys xs) labels
                                                          where pairs = zip (repeat y) labels
                                                                tupleRow = map (\(a,b)-> a b ) (zip (map replicate x) pairs)
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
