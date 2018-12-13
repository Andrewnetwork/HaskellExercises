-- ListManipulations.hs
-- Andrew Ribeiro
-- December 13, 2018

module ListManipulations where
import Data.Maybe

insert :: (Eq t1, Num t1) => t2 -> [t2] -> t1 -> [t2]
insert e [] pos = []
insert e (x:xs) pos
   | pos == 0 = e:xs
   | otherwise = x:(insert e (xs) (pos-1))

matrixInsert :: (Eq t1, Num t1) => t2 -> (Int, t1) -> [[t2]] -> [[t2]]
matrixInsert e (row,col) mat = insert (insert e (mat!!row) col) mat row

listNeighbors :: [b] -> [(b, b)]
listNeighbors ls = zip (init ls) (tail ls)

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

tupleToList :: (a, a) -> [a]
tupleToList (a,b) = [a,b]
