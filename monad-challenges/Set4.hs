{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where

import MCPrelude
import Test.DocTest
import Set2 hiding (test)

test = doctest ["Set4.hs"]

-- ## Problem 1 ##
-- General genTwo ~= link
-- m a -> (a -> m b) -> m b
-- General yLink ~= generalB
-- (a -> b -> c) -> m a -> m b -> m c
newtype Gen a = Gen {runGen :: Seed -> (a, Seed)}

class Monad m where
  bind :: m a -> (a -> m b) -> m b
  return :: a -> m a

evalGen :: Gen a -> Seed -> a
evalGen (Gen g) seed = fst $ g seed
-- | evalGen
-- >>> evalGen (Gen rand) (mkSeed 1)
-- 16807

instance Monad Maybe where
  return a = Just a
  bind Nothing _          = Nothing
  bind (Just val) maybeFn = maybeFn val
-- | Monad Maybe test
-- >>> return 4 :: Maybe Int
-- Just 4
-- >>> Just 3 `bind` (Just . (*5))
-- Just 15

instance Monad [] where
  return a = [a]
  bind ls fn = concat $ map fn ls
-- | Monad [] test
-- >>> [1..10] `bind` (return . (*2))
-- [2,4,6,8,10,12,14,16,18,20]
-- >>> [1..10] `bind` (\x -> if x < 5 then [x] else [] )
-- [1,2,3,4]
-- >>> return 3 :: [Int]
-- [3]

instance Monad Gen where
  return a = Gen (\s -> (a,s))
  bind gen fn = Gen (\s0 -> let (a,s1) = runGen gen s0
                            in runGen (fn a) s1)

testGenBind :: Gen (Seed -> ((Integer, Integer), Seed))
testGenBind = (Gen rand) `bind` (return . fn)
              where fn = (\x -> \s0 -> let (a,s1) = runGen (Gen rand) s0
                                       in ((x,a),s1) )
-- | Monad Gen test
-- runGen (return 3 :: Gen Int) (mkSeed 1)
-- (3,Seed {unSeed = 1})
-- runGen testGenBind (mkSeed 1)
-- rand (mkSeed 16807)

sequence :: (Monad m1, Monad m2) => m1 m2 -> m2 m1
sequence
-- repRandom2 :: [Gen a] -> Gen [a]
-- repRandom2 (a:as) = a `genTwo` (\x -> (repRandom2 as) `genTwo` (\y -> mkGen $ x:y) )
-- repRandom2 [] = mkGen []
