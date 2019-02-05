-- functorToFunctor.hs
-- Andrew Ribeiro
-- December 2018

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
import BinaryCounterTree

-- (FunctorCat a b) => f a -> f b
-- nmap :: (a->b) -> f1 a -> f2 b
-- class FunctorCat f f1 f2 | f f1 -> f2 where
--   nmap :: f -> f1 a -> f2  b

class FunctorCat f1 f2 where
  nmap :: (Ord a) => (a->b) -> f1 a -> f2 b

instance FunctorCat [] BinaryCounterTree where
  nmap f ls = fmap f $ listToTree ls

l1 = listToTree [1,2,3,3,3,3,4,4,5,6,6,6,6,6,6,6]

test1 :: BinaryCounterTree Integer
test1 = nmap (+1) [1,2,3]

-- functorMap :: (Functor f1, Functor f2) => (a->b) -> f1 a -> f2 b
-- functorMap fn f1 = f2f $ fmap fn f1
-- -- functorMap (+1)
-- f2f :: (Functor f1, Functor f2) => f1 -> f2
-- f2f f1 = f2
