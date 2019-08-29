{-|
Module: Numbers
Description: All about numbers.
Copyright: (c) Andrew Ribeiro 
Maintainer: andrewnetwork@gmail.com 
-}
module Numbers where
import           Data.Fixed (mod')
import           Data.List  (nub, sort)

------------------------------------------------------------------
------------------------- Integers -------------------------------
------------------------------------------------------------------

-- How Gauss originally thought about summing sequences of natural numbers.
-- i.e.:  Σ [1,2,3,4] = 
-- 1 2 3 4
-- + + + + = (5 + 5 + 5 + 5) / 2 = 20 / 2 = 10
-- 4 3 2 1 
-- More info: https://www.nctm.org/Publications/Teaching-Children-Mathematics/Blog/The-Story-of-Gauss/
gaussIntegerSum :: (Fractional p, Enum p) => p -> p
gaussIntegerSum n = sum tupSum / 2
                     where ls = [1..n]
                           tupSum = (\(a,b) -> a+b) <$> (zip ls (reverse ls))
-- >>> gaussIntegerSum 3
-- 6.0
--

gaussIntegerSumFormula :: Fractional a => a -> a
gaussIntegerSumFormula end = (end*(end+1))/2

mapSum :: Num a1 => [a2] -> [b] -> ((a2, b) -> a1) -> a1
mapSum a b f = sum $ map f (zip a b)
-- Given two sequences of integers a@[b..c] and d@[b+1..c+1] what is
-- the average sum of these two sequences?
-- Answer:
averageSumSeq :: Fractional a => [a] -> [a] -> a
averageSumSeq a d = (mapSum a d (\(x,y)->x+y))/2
-- Example: averageSumSeq [1..10] [2..11]

genSeqs :: (Enum a, Num a) => a -> a -> ([a], [a])
genSeqs b c = ([b..c],[b+1..c+1])

averageSumSeqProof :: (Eq a, Fractional a) => [a] -> [a] -> Bool
averageSumSeqProof a d = (averageSumSeq a d) == (sum a + sum d)/2
inductiveProof = all (True==) $ map (\(a,d) -> averageSumSeqProof a d) (map (\(b,c)->genSeqs b c) [ (x,y) | x <- [1..100], y<-[1..100],x<y])
-- averageSumSeqProof [1..10] [2..11]


-- [ y | x <- map genSeqs [1..100], y<- map x [1..100]]
--

-- So gauss had some internal function that enabled him to do: gaussIntegerSum -> gaussIntegerSumFormula
-- Which we note is (sum $ map (\(a,b) -> a+b) (zip ls (reverse ls))) -> (end*(end+1))

collatz :: Int -> Int
collatz n
  | mod n 2 == 0 = quot n 2
  | otherwise = 3*n + 1

collatzSeq n
  | n == 1 = [1]
  | otherwise = n:collatzSeq res
  where res = collatz n
-- btreeSort True $ collatzSeq $ 30
-- btreeSort False $ collatzSeq $ 30
-- btreeSort False $ collatzSeq $ 10000

-- listToTree $ collatzSeq 10

-- Takes a sequence of numbers and create pools of numbers that are in the same
-- pooling distance.
-- poolSequence ls poolDist = sort ls

poolSequence ls poolDist = poolSequence' ls poolDist uniqueNums
                           where uniqueNums = sort.nub $ ls


poolSequence' ls poolDist [] = ls
poolSequence' ls poolDist (x:xs) = poolSequence' ps poolDist xs
                                   where ps = poolSequence'' ls x poolDist


poolSequence'' [] poolRoot poolDist = []
poolSequence'' (x:xs) poolRoot poolDist
  | abs (x-poolRoot) <= poolDist = poolRoot:nextElm
  | otherwise = x:nextElm
  where nextElm = poolSequence'' (xs) poolRoot poolDist

--poolSequence [1,2,2,1,10,11,10] 2-> [1,1,1,1,2,2,2]
--
