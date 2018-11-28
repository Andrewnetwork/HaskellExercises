-- sorting.hs
-- November 2018
-- Andrew Ribeiro
-- In this exercise we will explore the traditional problem of sorting
-- a list of numbers from the perspective of types.

---- ### Types ###
-- Ordered Pair type.
-- E: equal, L: fst is less than snd, G: fst is greater than snd
data OP = E Int Int | L Int Int | G Int Int deriving Show

---- ### Utility Functions ###
rotate' :: [a] -> [a]
rotate' ls = [last ls] ++ init ls
-- rotate' [1,2,3,4] -> [4,1,2,3]

rotate :: (Eq t, Num t) => [a] -> t -> [a]
rotate ls n
    | n == 0 = ls
    | otherwise = rotate (rotate' ls) (n-1)

makePairs' :: [Int] -> Int -> [(Int,Int)]
makePairs' ls n
  | n == 0 = []
  | otherwise = zip ls (rotate ls n)  ++ makePairs' ls (n-1)

makePairs :: [Int] -> [(Int, Int)]
makePairs ls = makePairs' ls ((length ls)-1)

createPairs :: [b] -> [(b, b)]
createPairs ls = zip (reverse (tail (reverse ls))) (tail ls)

lte :: Ord a => (a, a) -> Bool
lte pair = fst pair <= snd pair

pairWiseBoolean ls = (map lte (createPairs ls))
-- pairWiseBoolean [1,2,3,4]

pwbp ls = zip (pairWiseBoolean ls) (createPairs ls)
-- pwbp [1,2,4,3]

replaceMap (a,b) =
    \x -> if a == x then b else (if b == x then a else x )
-- (replaceMap (1,2)) 1 -> 2
-- (replaceMap (1,2)) 2 -> 1
-- (replaceMap (1,2)) 3 -> 3

---- ### Type Transformations ###
-- Convert list of tuples to list of ordered tuples.
makeOrderedPairs :: [(Int,Int)] -> [OP]
makeOrderedPairs [] = []
makeOrderedPairs (x:xs)
    | f < s = [L f s]++makeOrderedPairs xs
    | f > s = [G f s]++makeOrderedPairs xs
    | f == s = [E f s]++makeOrderedPairs xs
    where f = fst x
          s = snd x
-- makeOrderedPairs (makePairs [1,2,3,4])

isSorted :: [Int]-> Bool
isSorted (x:xs)
    | xs == [] = True
    | otherwise = (x <= head(xs)) && isSorted(xs)
-- isSorted [1,2,3,4] -> True

swapFTuples [] = []
swapFTuples (x:xs)
    | fst x = swapFTuples(xs)
    | otherwise = [snd x] ++ swapFTuples(xs)
-- swapFTuples (pwbp [1,2,4,3]) -> [(4,3)]

replaceMaps ls = (map replaceMap (swapFTuples (pwbp ls) ))

-- TODO: Major issue here.
applyReplaceMaps [] ls     = ls
applyReplaceMaps (x:xs) ls = applyReplaceMaps xs (map x ls)
-- applyReplaceMaps' (replaceMaps [1,2,4,3]) [1,2,4,3]

sortByReplace ls = applyReplaceMaps (replaceMaps ls) ls
-- sortByReplace [1,3,4,2]
-- sortByReplace (sortByReplace [1,3,4,2])
-- sortByReplace [4,2,3,1]

sortByReplace' ls
    | isSorted (sortByReplace ls) = [sortByReplace ls]
    | otherwise = [(sortByReplace ls)] ++ sortByReplace' (sortByReplace ls)
-- sortByReplace' [4,2,3,1]
-- sortByReplace' [7,6,5,4,3,2,1]
-- sortByReplace' [4,1,2,3,7,5,6]
-- sortByReplace' [1,2,2,3,4,6,5] 
