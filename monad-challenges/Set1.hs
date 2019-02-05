{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude
import Test.DocTest

test = doctest ["Set1.hs"]

-- rand :: Seed -> (Integer, Seed)
-- mkSeed :: Integer -> Seed

-- ## Problem 1 ##
fiveRands :: [Integer]
fiveRands = firstRand : secondRand : thirdRand : fourthRand : fifthRand : []
            where (firstRand,firstSeed)    = rand (mkSeed 1)
                  (secondRand,secondSeed)  = rand firstSeed
                  (thirdRand,thirdSeed)    = rand secondSeed
                  (fourthRand,fourthSeed)  = rand thirdSeed
                  (fifthRand,_)            = rand fourthSeed
-- | fiveRands
-- >>> product fiveRands
-- 8681089573064486461641871805074254223660

-- ## Problem 2 ##
type Gen a = Seed -> (a, Seed)
type GenChain a b =  (a -> Gen b)

randLetter :: Gen Char
randLetter seed = (toLetter randomNumber,nextSeed)
                  where (randomNumber, nextSeed) = rand seed
-- | randLetter
-- >>> randLetter (mkSeed 1)
-- ('l',Seed {unSeed = 16807})

randString3 :: String
randString3 = firstRand : secondRand : thirdRand : []
              where (firstRand,firstSeed)    = randLetter (mkSeed 1)
                    (secondRand,secondSeed)  = randLetter firstSeed
                    (thirdRand,thirdSeed)    = randLetter secondSeed
-- | randString3
-- >>> randString3
-- "lrf"

-- ## Problem 3 ##
generalA :: (a->b) -> Gen a -> Gen b
generalA fn gen = \seed -> let (a,b) = gen seed
                           in (fn a,b)
-- | generalA
-- >>> generalA id rand (mkSeed 1)
-- (16807,Seed {unSeed = 16807})

generalA1 :: (a,b) -> (a->c) -> (c,b)
generalA1 (a,b) fn = (fn a,b)

generalA2 :: (a->b) -> Gen a -> Gen b
generalA2 fn gen = helper.gen
                   where helper (a,b) = (fn a,b)
-- generalA2 (+2) (rand) (mkSeed 1)
gmap = generalA

randEven :: Gen Integer -- the output of rand * 2
randEven seed = (randomNumber*2,nextSeed)
                where (randomNumber, nextSeed) = rand seed

randEven1 :: Gen Integer -- the output of rand * 2
randEven1 seed = generalA1 (rand seed) (*2)

randEven2 :: Gen Integer -- the output of rand * 2
randEven2 seed = generalA (*2) rand seed

randEven3 :: Gen Integer -- the output of rand * 2
randEven3 = generalA (*2) rand

-- randEven2 (mkSeed 1)

randOdd :: Gen Integer -- the output of rand * 2 + 1
randOdd seed = (evenRandomNumber + 1, nextSeed)
               where (evenRandomNumber, nextSeed) = randEven seed

randOdd1 :: Gen Integer -- the output of rand * 2 + 1
randOdd1 seed = generalA1 (randEven seed) (+1)

randOdd2 :: Gen Integer -- the output of rand * 2 + 1
randOdd2 seed = generalA (+1) randEven seed


randTen :: Gen Integer -- the output of rand * 10
randTen seed = let (randomNumber, nextSeed) = rand seed
               in  (randomNumber*10,nextSeed)

randTen1 :: Gen Integer -- the output of rand * 10
randTen1 seed = generalA1 (rand seed) (*10)

-- ## Problem 4 ##
-- type Gen a = Seed -> (a, Seed)
randPair :: Gen (Char, Integer)
randPair seed = let (randomLeter, firstSeed)   = randLetter seed
                    (randomNumber, secondSeed) = rand firstSeed
                in  ((randomLeter,randomNumber), secondSeed)
-- randPair (mkSeed 1)

generalPair :: Gen a -> Gen b -> Gen (a,b)
generalPair genA genB =  \seed -> let (pairValue1, pairSeed1) = genA seed
                                      (pairValue2, pairSeed2) = genB pairSeed1
                                  in ((pairValue1,pairValue2),pairSeed2)
-- generalPair randLetter rand (mkSeed 1)

generalB :: (a->b->c) -> Gen a -> Gen b -> Gen c
generalB fn genA genB = \seed -> let (pairValue1, pairSeed1) = genA seed
                                     (pairValue2, pairSeed2) = genB pairSeed1
                                 in  (fn pairValue1 pairValue2,pairSeed2)
-- generalB (,) rand rand (mkSeed 1)

-- genTwo :: Gen a -> (a -> Gen b) -> Gen b
generalB2 :: (a->b->c) -> Gen a -> Gen b -> Gen c
generalB2 fn genA genB = genA `genTwo` (genProd fn genB)
-- generalB2 (,) randLetter rand (mkSeed 1)
-- generalB2 (,) randLetter randLetter (mkSeed 1)
-- generalB2 (flip (:)) (generalB2 (flip (:)) (generalB2 (\a b -> a:b:[]) rand rand) rand) rand (mkSeed 1)
-- generalB ~= generalB2
-- generalB2 (flip (:)) (generalB2 (\a b -> a:b:[]) rand rand ) rand (mkSeed 1)

generalPair2 :: Gen a -> Gen b -> Gen (a,b)
generalPair2 = generalB (,)
-- generalPair2 randLetter rand (mkSeed 1)
-- generalPair2 randLetter randLetter (mkSeed 1)

-- ## Problem 5 ##
repRandom :: [Gen a] -> Gen [a]
repRandom (a:as) = a `genTwo` (\x -> generalA (\y -> x:y) (repRandom as) )
repRandom [] = mkGen []
-- repRandom (replicate 34 randLetter) (mkSeed 1)

--repRandom2 :: [Gen a] -> Gen [a]
repRandom2 :: [Gen a] -> Gen [a]
repRandom2 (a:as) = a `genTwo` (\x -> (repRandom2 as) `genTwo` (\y -> mkGen $ x:y) )
repRandom2 [] = mkGen []
-- repRandom2 (replicate 34 randLetter) (mkSeed 1)

repRandom1 :: [Gen a] -> Gen [a]
repRandom1 (gen:gens) = \seed -> let (randomValue,randomSeed) = gen seed
                                 in let (nextRandomValue,nextSeed) = repRandom1 gens randomSeed
                                    in (randomValue:nextRandomValue,nextSeed)
repRandom1 [] = \seed -> ([],seed)

-- ## Problem 6 ##
-- type Gen a = Seed -> (a, Seed)
-- genTwo is our bind >>=
genTwo :: Gen a -> GenChain a b -> Gen b
genTwo gen genProd = \s0 -> let (a,s1) = gen s0
                            in genProd a s1
-- genTwo rand tst (mkSeed 1)
-- genTwo rand tst2 (mkSeed 1)


tst :: GenChain a (a, Integer)
tst a = \s -> let (r,s1) = rand s
              in ((a,r),s1)

tst2 :: GenChain a (a, Integer)
tst2 = genProd (,) rand
-- tst ~= tst2

genProd :: (c -> b -> a) -> Gen b -> GenChain c a
genProd construct gen prev = \s -> let (a,s2) = gen s
                                   in (construct prev a,s2)
-- genProd (,) rand 3 (mkSeed 1)
-- genProd (\a b -> a:b:[]) rand 3 (mkSeed 1)
-- (\a b -> a:b:[]) 1 2
-- genProd (,) rand 3 (mkSeed 1)

mkGen :: a -> Gen a
mkGen a = \seed -> (a,seed)
