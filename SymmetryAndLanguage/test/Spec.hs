import Test.Hspec

main :: IO ()
main = hspec $ do
               tests1





               --  map (\x->x [[1,0],[1,1]] )  (map symPathToFunc (symPath [[1,0],[1,1]] [[1,1],[1,0]]))
               --  map ((\x->x [[1,0],[1,1]] ).symPathToFunc) (symPath [[1,0],[1,1]] [[1,1],[1,0]])
               -- getSymPaths [] (symTree I [[0,0,1],[0,1,0],[1,0,1]] [])
               -- getSymPaths [] (symTree I [[1,2,3],[4,5,6],[7,8,9]] [])
               -- [[1,2,3],[4,5,6],[7,8,9]] -> [[7,4,1],[8,5,2],[9,6,3]] | [HS,VS,CS,VS,HS,VS,I]
               --

               -- putStrLn (concatMap printMatrix (getBoards (symTree I [[0,0,1],[0,1,0],[1,0,1]] [])))
               -- lenSymTree (symTree I [[1,2,3],[4,5,6],[7,8,9]] [])
               -- lenSymTree (symTree I [[1,2],[3,4]] [])
               -- lenSymTree (symTree I [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]] [])

               -- [1,0,1],
               -- [0,1,0],
               -- [1,0,1]

               -- [[0,1],
               --  [1,0]]

               --  [1,0,1],
               --  [0,1,0]

               -- cdiag [[0,1,0],[0,0,1],[0,0,0]]

               -- [[0,0,0],
               --  [0,0,1],
               --  [0,1,0]]

               -- symTree I [[1,2,3],[4,5,6],[7,8,9]] []
               -- [[[1,2],[3,4]],[[5,6],[7,8]]]++[[9,10],[11,12]]
               -- map (\x -> x OP (depth-1)) (map symTree reflections)
               -- ($mat) <$> [diag,cdiag,vert,horz]
               -- children =  map (\x -> x OP (depth-1)) (map symTree reflections
               -- ($ [[1,2,3],[4,5,6],[7,8,9]]) <$> [diag,cdiag,vert,horz]
               --elem mat hist = Empty
               -- children = map (gameTree (otherPlayer player)) boards
               -- TODO: Local Symmetry
               -- strSymConds$symFlags$[[0,1,0],[0,0,1],[0,0,0]]
               -- strSymConds$symFlags$[[0,1],[1,0]]
               -- symTree I [[1,2],[3,4]] []

               -- verifyPaths [[1,2,3],[4,5,6],[7,8,9]] [[7,4,1],[8,5,2],[9,6,3]] (symPath [[1,2,3],[4,5,6],[7,8,9]] [[7,4,1],[8,5,2],[9,6,3]])


-- symPathToFunc [VS] [[0,0,1],[0,1,0],[1,0,1]]

               -- symPath [[1,2,3],[4,5,6],[7,8,9]] [[7,4,1],[8,5,2],[9,6,3]]
               -- symPath [[1,0],[1,1]] [[3,3],[3,3]]
               -- symFlags [[E,(P X),E],[(P X),E,(P X)],[(P X),E,(P X)]]
               -- symFlags $ chunksOf 3 [0,1,0,1,0,1,0,1,0]



-- cdiag [[(P X),(P O),(P X)],[(P X),(P O),(P X)],[(P O),(P X),(P O)]]
               -- diag [[(P X),(P O),(P X)],[(P X),(P O),(P X)],[(P O),(P X),(P O)]]
               -- diag [[(P X),E,E],[E,(P X),E],[E,E,(P X)]]
               -- diag [[E,E,(P X)],[E,(P X),E],[(P X),E,E]]
               -- diag [[E,(P X),(P O)],[E,(P X),E],[(P X),(P O),E]]

               -- strSymConds (symFlags [[E,(P X),E],[(P X),E,(P X)],[(P X),E,(P X)]])
               -- ('┌', '─', '┬', '┐'), ('├', '─', '┼', '┤'); , └', '─', '┴', '┘')│';
               -- Symmetry [[E,(P X),E],[(P X),E,(P X)],[(P X),E,(P X)]] (symmetries [[E,(P X),E],[(P X),E,(P X)],[(P X),E,(P X)]])
               -- Symmetry (chunksOf 3 [0,1,0,1,0,1,0,1,0]) (symmetries $ chunksOf 3 [0,1,0,1,0,1,0,1,0])



               -- vert [[(P X),E,E],[E,(P X),E],[E,E,(P X)]]
               -- map (\[a,b,c] -> [c,b,a]) board





 -- horz [[(P X),E,E],[E,(P X),E],[E,E,(P X)]]
 -- horz [[(P X),E,E],[E,(P X),E],[E,E,(P X)]]
 -- horz [[(P O),(P X),(P O)],[(P X),(P O),(P X)],[(P O),(P X),(P O)]]
 -- horz [[(P X),(P O),(P X)],[(P X),(P O),(P X)],[(P O),(P X),(P O)]]

 -- treeToList.listToTree $ [1,1,4,5,6]
               -- treeToList.listToTree $ [6,5,4,3,2]
               -- treeToList.listToTree $ [1,1,-1,-2,4,9,5,6]
-- insertIntoTree 4 (insertIntoTree 3 None)
-- insert 0 [1,2,3,4] 2
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
 -- adjMatrixToSeq :: AdjacencyMatrix -> [Int]
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
