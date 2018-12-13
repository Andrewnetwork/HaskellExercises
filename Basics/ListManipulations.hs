module ListManipulations where
import Data.Maybe

insert :: (Eq t1, Num t1) => t2 -> [t2] -> t1 -> [t2]
insert e [] pos = []
insert e (x:xs) pos
   | pos == 0 = e:xs
   | otherwise = x:(insert e (xs) (pos-1))
-- insert 0 [1,2,3,4] 2

matrixInsert :: (Eq t1, Num t1) => t2 -> (Int, t1) -> [[t2]] -> [[t2]]
matrixInsert e (row,col) mat = insert (insert e (mat!!row) col) mat row

listNeighbors :: [b] -> [(b, b)]
listNeighbors ls = zip (init ls) (tail ls)
-- listNeighbors [1,2,3,4,5,6]

listNeighborsInv :: [(a, a)] -> [a]
listNeighborsInv (x:xs) = (tupleToList x)++listNeighborsInv' xs
listNeighborsInv' ((a,b):xs) = b:listNeighborsInv' xs
listNeighborsInv' [] = []

replaceElement :: Eq b => b -> b -> [b] -> [b]
replaceElement e ne ls = map (\x-> if e == x then ne else x) ls

locations :: (Eq t, Num a) => t -> [t] -> [a]
locations e ls = locations' e ls 0
locations' e [] pos = []
locations' e (x:xs) pos
  | x == e = pos:locations' e xs (pos+1)
  | otherwise = locations' e xs (pos+1)

concatOnHead :: Eq t => [t] -> [t] -> [t]
concatOnHead ls cls = concatOnHead' ls cls (locations (head cls) ls) 0
concatOnHead' ls cls [] count = ls
concatOnHead' ls cls (x:xs) count = concatOnHead' concatInstance cls xs (count+1)
                                    where splitPair = (splitAt (x+((length cls)-1)*count) ls)
                                          concatInstance = (fst splitPair) ++ cls ++ (tail.snd $ splitPair)
-- concatOnHead [1,2,3,4,5,3,9,9,3] [3,0,0,0]

selectivePop :: [a] -> (a -> Bool) -> (Maybe a, [a])
selectivePop ls fn = selectivePop' ls [] fn
-- selectivePop [1,2,3,4] (\x->x==3) -> (3,[1,2,4])

selectivePop' :: [a] -> [a] -> (a -> Bool) -> (Maybe a, [a])
selectivePop' [] ls fn = (Nothing,ls)
selectivePop' (x:xs) ls fn
  | fn x = (Just x,ls++xs)
  | otherwise = selectivePop' xs (ls++[x]) fn
-- selectivePop' [1,2,3,4] [0,0,0] (\x->x==3)

-- filter (\x->x/=3) [1,2,3,4,5]
--selectivePop ls fn =
-- selectivePop [1,2,3,4,5] (\x->3=x) -> (3,[1,2,4,5])
-- selectivePop [1,2,3,4,5] (\x->x>3) -> (4,[1,2,3,5])
-- concatOnHead [1,2,3,4,5,6] [3,5,6] -> [1,2,3,5,6,4,5,6]
-- concatOnHead [1,2,3,4,5,3] [3,5,6] -> [1,2,3,5,6,4,5,3,5,6]
-- concatOnHead [1,2,4,1,2,3,4,1,2,3,4] [2,0]
-- concatOnHead [2,4,1,2,3,4,1,2] [2,0]
-- splitAt 3  [0,1,2,3,4,5,6,7]
-- map (\x-> splitAt x [1,2,3,4,2,1,4]) $ locations 2 [1,2,3,4,2,1,4]
--mergeTupleList :: Eq t => [(t,t)] -> [t]
--mergeTupleList ls =
-- listNeighbors [1,2,3,4,5,6] -> [(1,2),(2,3),(3,4),(4,5),(5,6)]
-- listNeighbors.listNeighbors $ [1,2,3,4,5,6]
--listNeighborsInv ls = map (\((a,b),(c,d))->[a,b,d]) (listNeighbors ls)
-- listNeighborsInv [(1,2),(2,3),(3,4),(4,5),(5,6)]
--listNeighborsInv [] = []
--listNeighborsInv ((a,b):(c,d):xs) = [a,b,d]:listNeighborsInv xs
--mergeTuples (a,b) (c,d) = (a,b,d)
tupleToList :: (a, a) -> [a]
tupleToList (a,b) = [a,b]

-- listNeighborsInv [(1,2),(2,3),(3,4),(4,5),(5,6)]
-- seqAdjacency [1,3,2,1,3,2,1,3,2,2,1,3]
