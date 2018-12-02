-- [0,1,2,3,4,5,6,7,8] -> [[0,1,2],[3,4,5],[6,7,8]]
isRowWin boardState = any (\[a,b,c] -> (a == b) && (b == c) && a /= E) (chunksOf 3 boardState)
--isRowWin boardState = foldl (||) False (map (\[a,b,c] -> (a == b) && (b == c) /= E ) (chunksOf 3 boardState))
-- isRowWin [(P X),(P X),(P O),(P X),(P X),(P O),(P X),(P X),(P O)]
-- isRowWin [(P X),(P X),(P X),(P X),(P X),(P O),(P X),(P X),(P O)]
-- transpose (chunksOf 3  [0,1,2,3,4,5,6,7,8]) 
-- [0,1,2,
--  3,4,5,
--  6,7,8]

-- [[0,1,2],[3,4,5],[6,7,8]]
-- [[2,1,0],[5,4,3],[8,7,6]]

whoWonColumn boardState = (head.head) (filter (\[a,b,c] -> (a == b) && (b == c) && a /= E) 
                          (transpose (chunksOf 3 boardState)))

whoWonColumn boardState = (head.head) (filter (\[a,b,c] -> (a == b) && (b == c) && a /= E) (transpose boardState))

