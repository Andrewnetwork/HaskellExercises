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

adjMatrixPairs mat@(AdjacencyMatrix labels _ ) = foldr (++) [] $ foldr (++) [] (adjMatrixPairs' mat labels)
adjMatrixPairs' (AdjacencyMatrix _ [] ) labels = []
adjMatrixPairs' (AdjacencyMatrix (y:ys) (x:xs) ) labels = tupleRow : adjMatrixPairs' (AdjacencyMatrix ys xs) labels
                                                          where pairs = zip (repeat y) labels
                                                                tupleRow = map (\(a,b)-> a b ) (zip (map replicate x) pairs)

adjMatrix (AdjacencyMatrix _ matrix) = matrix

-- ### Transformations of AdjacencyMatrix ###
adjMatrixToSeqClass :: AdjacencyMatrix -> [[Int]]
adjMatrixToSeqClass adjMat = combinations $ map tupleToList (adjMatrixPairs adjMat)

addAdjacency :: Int -> Int -> AdjacencyMatrix -> AdjacencyMatrix
addAdjacency from to (AdjacencyMatrix labels mat) = AdjacencyMatrix labels (matrixInsert weight (fromLoc,toLoc) mat)
                                                    where fromLoc = fromJust $ elemIndex from labels
                                                          toLoc = fromJust $ elemIndex to labels
                                                          weight = (mat!!fromLoc!!toLoc + 1)
-- ### Transformations to AdjacencyMatrix ###
applySym :: ([[Int]] -> [[Int]]) -> [Int] -> AdjacencyMatrix
applySym symmetryFn ls = AdjacencyMatrix labels (symmetryFn $ matrix)
                         where (AdjacencyMatrix labels matrix) = seqAdjacency ls

initAdjacencyMatrix :: [Int] -> AdjacencyMatrix
initAdjacencyMatrix ls = AdjacencyMatrix uniqueElms (replicate uniqueElmsLen (replicate uniqueElmsLen 0))
                         where uniqueElms = (treeNub ls)
                               uniqueElmsLen = (length uniqueElms)

seqAdjacency :: [Int] -> AdjacencyMatrix
seqAdjacency ls = seqAdjacency' (listNeighbors ls) (initAdjacencyMatrix ls)
seqAdjacency' [] adjMat         = adjMat
seqAdjacency' ((a,b):xs) adjMat = seqAdjacency' xs (addAdjacency a b adjMat)

-- ### Symmetry Paths ###
adjMatrixSymmetryPath :: AdjacencyMatrix -> AdjacencyMatrix -> [SymmetryPath]
adjMatrixSymmetryPath (AdjacencyMatrix _ source) (AdjacencyMatrix _ dest) = symPath source dest

seqSymmPath :: [Int] -> [Int] -> [SymmetryPath]
seqSymmPath s1 s2 = adjMatrixSymmetryPath (seqAdjacency s1) (seqAdjacency s2)
-- ### Lower Level Transformations ###
sequenceClass :: [Int] -> [[Int]]
sequenceClass ls = combinations $ map tupleToList (adjMatrixPairs.seqAdjacency $ ls)

symmetryCousins :: ([[Int]] -> [[Int]]) -> [Int] -> [[Int]]
symmetryCousins symmetryFn ls = adjMatrixToSeqClass (applySym symmetryFn ls)

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

adjacencyMatrixTree :: [Int] -> [[Int]]
adjacencyMatrixTree ls = nub $ concatMap adjMatrixToSeqClass adjMatrixList
                         where (AdjacencyMatrix label mat) = (seqAdjacency ls)
                               symmetryTree = makeSymTree mat
                               adjMatrixList = map (AdjacencyMatrix label) (getParents symmetryTree)


-- ####### INSTANCES AND IO ############
instance Show AdjacencyMatrix where
  show (AdjacencyMatrix labels matrix) = gridString matrix

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
