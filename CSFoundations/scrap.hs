-- Reversing a list:
reverseLs [x] = [x]
reverseLs (x:xs) = (reverseLs xs)++[x]
-- Examples:
-- reverseLs [1,2,3]
-- reverseLs [ [1,2],[3,4],[5,6] ]
-- map reverseLs [ [1,2],[3,4],[5,6] ]
-- reverseLs (map reverseLs [ [1,2],[3,4],[5,6] ]) == map reverseLs (reverseLs [ [1,2],[3,4],[5,6] ])

rotate' ls = [last ls] ++ reverse( tail (reverse ls) )
rotate ls n
    | n == 0 = ls
    | otherwise = rotate (rotate' ls) (n-1)
-- rotate' [1,2,3]
-- rotate' (rotate' [1,2,3])
-- rotate' ( rotate' (rotate' [1,2,3]) )
-- rotate [1,2,3,4] 2

makePairs' :: [Int] -> Int -> [(Int,Int)]
makePairs' ls n
  | n == 0 = []
  | otherwise = zip ls (rotate ls n)  ++ makePairs' ls (n-1)

makePairs ls = makePairs' ls ((length ls)-1)
-- makePairs [1,2,3,4]

data OP = E Int Int | L Int Int | G Int Int deriving Show

-- Convert list of tuples to list of ordered tuples.
makeOrderedPairs :: [(Int,Int)] -> [OP]
makeOrderedPairs (x:xs)
    | f < s = [L f s]++makeOrderedPairs xs
    | f > s = [G f s]++makeOrderedPairs xs
    | f == s = [E f s]++makeOrderedPairs xs
    where f = fst x
          s = snd x
makeOrderedPairs [] = []
-- makeOrderedPairs (makePairs [1,2,3,4])

typeSignature (L a b) = "L"
typeSignature (G a b) = "G"
typeSignature (E a b) = "E"


-- Types of sorted lists:
-- map typeSignature (makeOrderedPairs (makePairs [1,2,3,4]))
-- ["L","L","L","G","L","L","G","G","L","G","G","G"]

-- map typeSignature (makeOrderedPairs (makePairs [1,2,3]))
-- ["L","L","G","L","G","G"]

-- ["L","L","L","G","L","L","G","G","L","G","G","G"]
-- ["G","G","G","L","G","G","L","L","G","L","L","L"]

-- map typeSignature (makeOrderedPairs (makePairs [4,3,2,1]))
-- ["G","G","G","L","G","G","L","L","G","L","L","L"]
-- map typeSignature (makeOrderedPairs (makePairs [4,3,2,1]))
-- ["G","G","G","L","G","G","L","L","G","L","L","L"]

-- ["L","G"]
-- ["L","L","G","L","G","G"]
-- ["L","L","L","G","L","L","G","G","L","G","G","G"]

-- So we see that a list has a type signature. Sorting a list
-- reduces to transforming the type of the list to that of a sorted list.

isSorted :: [Int]-> Bool
isSorted (x:xs)
    | xs == [] = True
    | otherwise = (x <= head(xs)) && isSorted(xs)

createPairs ls = zip (reverse (tail (reverse ls))) (tail ls)
lte pair = fst pair <= snd pair

-- map lte (createPairs [1,2,3])
-- map lte (createPairs [1,3,2,4,5])
-- map lte [(1,1),(1,2),(2,1)] -> [True,True,False]
-- A false indicates a swap must be made in that tuple.
-- (map lte (createPairs [1,3,2,4]))

--swap (a,b) = (b,a)

--swappingPairMask mask ls = zip (createPairs ls)

-- swappingMask (map lte (createPairs [1,3,2,4])) (createPairs [1,3,2,4])

pairWiseBoolean ls = (map lte (createPairs ls))
-- pairWiseBoolean [1,2,3,4]

pwbp ls = zip (pairWiseBoolean ls) (createPairs ls)
-- pwbp [1,2,4,3]

replaceMap (a,b) =
    \x -> if a == x then b else (if b == x then a else x )
-- (replaceMap (1,2)) 1 -> 2
-- (replaceMap (1,2)) 2 -> 1
-- (replaceMap (1,2)) 3 -> 3

swap (a,b) (c,d) = ((replaceMap (a,b)) c , (replaceMap (a,b)) d)
-- swap (1,2) (2,3) -> (1,3)
-- swap (1,2) (1,2) -> (2,1)

swapFTuples [] = []
swapFTuples (x:xs)
    | fst x = swapFTuples(xs)
    | otherwise = [snd x] ++ swapFTuples(xs)
-- swapFTuples (pwbp [1,2,4,3]) -> [(4,3)]

replaceMaps ls = (map replaceMap (swapFTuples (pwbp ls) ))

applyReplaceMaps [] ls = ls
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
-- sortByReplace' [4,6,3,2,5,1]
-- sortByReplace' [6,5,4,3,2,1]
-- sortByReplace' [6,5,4,3,2,1]

-- swapFTuples (pwbp [4,2,3,1])

-- applyReplaceMaps' replaceMaps ls = (map replaceMap (swapFTuples (pwbp ls) )) [1,2,4,3]


-- (replaceMap (1,2)) 1 ->
-- map (replaceMap (3,4)) [3,2,2,3]
-- map (replaceMap (3,4)) (map (replaceMap (3,4)) [3,2,2,3])

import System.Random
randomList 0 = []
randomList n = [randomRIO (1, 10)::Int] ++ randomList (n-1)

-- sortByReplace' (randomList 6)
