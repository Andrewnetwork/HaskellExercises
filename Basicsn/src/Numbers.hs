{-|
Module: Numbers
Description: All about numbers.
Copyright: (c) Andrew Ribeiro 
Maintainer: andrewnetwork@gmail.com 
-}
module Numbers where
import           Data.Fixed                     ( mod' )
import           Data.List                      ( nub
                                                , sort
                                                , unionBy
                                                )
import           Test.LeanCheck.Stats           ( counts )
import           Test.DocTest
import Lists (maxValues)
------------------------------------------------------------------
------------------------- Integers -------------------------------
------------------------------------------------------------------

-- | The average of two two sequences. 
-- >>> averageSumSeq [1..10] [2..11]
-- 60.0
--
averageSumSeq :: Fractional a => [a] -> [a] -> a
averageSumSeq a b = sum (zipWith (+) a b) / 2


-- | How Gauss originally thought about summing sequences of natural numbers.
-- i.e.:  Σ [1,2,3,4] = 
-- 1 2 3 4
-- + + + + = (5 + 5 + 5 + 5) / 2 = 20 / 2 = 10
-- 4 3 2 1 
-- >>> gaussIntegerSum 4
-- 10.0
--
-- >>> (gaussIntegerSumFormula 12) == (gaussIntegerSum 12)
-- True
--
gaussIntegerSum :: (Fractional p, Enum p) => p -> p
gaussIntegerSum n = averageSumSeq ls (reverse ls) where ls = [1 .. n]

gaussIntegerSumFormula :: Fractional a => a -> a
gaussIntegerSumFormula x = x * (x + 1) / 2


collatz :: Int -> Int
collatz n | mod n 2 == 0 = quot n 2
          | otherwise    = 3 * n + 1

collatzSeq n | n == 1    = [1]
             | otherwise = n : collatzSeq res
    where res = collatz n
-- btreeSort True $ collatzSeq $ 30
-- btreeSort False $ collatzSeq $ 30
-- btreeSort False $ collatzSeq $ 10000

-- listToTree $ collatzSeq 10

-- Takes a sequence of numbers and create pools of numbers that are in the same
-- pooling distance.
-- poolSequence ls poolDist = sort ls

poolSequence ls poolDist = poolSequence' ls poolDist uniqueNums
    where uniqueNums = sort . nub $ ls


poolSequence' ls poolDist []       = ls
poolSequence' ls poolDist (x : xs) = poolSequence' ps poolDist xs
    where ps = poolSequence'' ls x poolDist


poolSequence'' [] poolRoot poolDist = []
poolSequence'' (x : xs) poolRoot poolDist
    | abs (x - poolRoot) <= poolDist = poolRoot : nextElm
    | otherwise                      = x : nextElm
    where nextElm = poolSequence'' (xs) poolRoot poolDist

--poolSequence [1,2,2,1,10,11,10] 2-> [1,1,1,1,2,2,2]
--

------------------------------------------------------------------
-------------------------- Primes --------------------------------
-- 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 -                                                        ..--
------------------------------------------------------------------

-- | `eSieve` `n` `s` implements the Sieve of Eratosthenes. 
-- >>> eSieve [2..10] 2
-- [2,3,5,7]
--
eSieve :: [Int] -> Int -> [Int]
eSieve [] _ = []
eSieve n  s = s : eSieve k (head k) where k = [ a | a <- n, mod a s /= 0 ]

-- | Generate primes. 
-- >>> take 20 primes
-- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71]
--
primes :: [Int]
primes = eSieve [2 ..] 2

-- | Prime Factorization
-- >>> primeFactorization 423432562546
-- [(2,1),(7,1),(59,1),(15683,1),(32687,1)]
--
-- >>> primeFactorizationInv [(2,1),(7,1),(59,1),(15683,1),(32687,1)]
-- 423432562546
--
-- >>> (primeFactorizationInv $ primeFactorization 423432562546) == 423432562546
-- True
--
primeFactorization' :: Integer -> Integer -> [Integer]
primeFactorization' s x | s >= x   = [x]
                        | mod x s == 0 = s : primeFactorization' s (quot x s)
                        | otherwise    = primeFactorization' (succ s) x

primeFactorization :: Integer -> [(Integer, Int)]
primeFactorization = counts . primeFactorization' 2

primeFactorizationInv :: [(Integer, Int)] -> Integer
primeFactorizationInv = product . (uncurry (^) <$>)

-- | LCM(a,b) is the smallest positive integer that is divisible by a and b. 
leastCommonMultiple :: Integer -> Integer -> Integer
leastCommonMultiple a b = 
    primeFactorizationInv $ maxValues $ primeFactorization a++primeFactorization b


-- >>> leastCommonMultiple 98 4
-- 196
--
-- >>> primeFactorization 98
-- [(2,1),(7,2)]
--
-- >>> primeFactorization 4
-- [(2,2)]
--
-- >>> all (\(a,b)-> lcm a b == leastCommonMultiple a b) (zip (reverse [2..300]) [2..300])
-- True
--


-- >>> nub [1,2,3,3]
-- [1,2,3]
--
main = doctest ["-isrc", "src/Numbers.hs"]
