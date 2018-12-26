import Control.Applicative
import Data.Maybe

-- #### Some helpers ####
makeDataset :: (Num a, Enum a) => (a -> b) -> a -> [(a, b)]
makeDataset fn maxNum = zip [0..maxNum] $ fn <$> [0..maxNum]

sumOfNat :: Fractional a => a -> a
sumOfNat n  = (n * (n + 1)) / 2

sumOfNatLs :: (Enum b, Fractional b) => b -> [(b, b)]
sumOfNatLs maxNum = makeDataset sumOfNat maxNum
-- ######################

-- \x-> sumOfSumOfNatLs <$> [liftA2 (+) head last,\x->head x + last x]
-- liftA2 (+) head last $ [1,2,3]
-- (\x->head x + last x) $ [1,2,3]
-- fn ls = head ls + last ls
-- fn [1,2,3]

-- Why is it infered that the type of tst is exclusively defined
-- for integers, not Num in general?
tst :: Num c => [c] -> c
tst = liftA2 (+) head last
-- tst [1,2,3]

-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
-- (+) :: Num a => a -> a -> a
-- head :: [a] -> a
-- last :: [a] -> a
-- (liftA2 (+) head last) [1.2,3.4]

tst2 = liftA2 (-) last head
-- tst2 [1,2,3] == 2

tst3 = liftA2 (-) head last
-- tst3 [1,2,3] == -2

-- tst [1,]
-- tst [2.5,4.6]
-- liftA2 ::  (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]  -- liftA2 defined in Instance of Applicative for []
liftA2' :: (a -> b -> c) -> [a] -> [b] -> [c]
liftA2' f xs ys = [f x y | x <- xs, y <- ys]
-- liftA2' (\x y->(x,y)) [0,1,2] [0,1,2]

-- liftA2 ::  (a -> b -> c) -> f a -> f b -> f c      -- liftA2 defined in Applicative Class.
liftA2'' :: Applicative f => (a -> b -> c) -> (f a -> f b -> f c)
liftA2'' f x = (<*>) (fmap f x)
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- map ::               (a -> b) -> [a] -> [b]
-- [1,2,3]

-- Space of natural numbers: [0,1..]
-- Space of multiples of two: map (*2) [0,1..]
-- take 100 $ map (*2) [0,1..]
-- take 100 $ map (*3) [0,1..]

-- We wish to consider types where the mapping operation does not
-- preserve structure.

-- Degenerate Functor : dimensionality reduction
class DegenerateFunctor d where
  dmap :: (a -> Maybe b) -> d a -> d b

instance DegenerateFunctor [] where
  dmap fn ls = filterFn $ fmap fn ls
               where filterFn :: [Maybe a] -> [a]
                     filterFn ((Just x):xs) = x : filterFn xs
                     filterFn (Nothing:xs)  = filterFn xs
                     filterFn [] = []

-- Generative Functor : dimensionality expansion
class GenerativeFunctor g where
  gmap :: (a -> [b]) -> g a -> g b

instance GenerativeFunctor [] where
  gmap fn ls = concatMap fn ls

evenMaybe :: Integral a => a -> Maybe a
evenMaybe x
  | mod x 2 == 0 = Just x
  | otherwise = Nothing

evenList :: Integral a => a -> [a]
evenList x
  | mod x 2 == 0 = [x]
  | otherwise = []

idAndPred x = [x,pred x]
-- idAndPred 10
-- (dmap evenMaybe [0..40]) == (map (*2) [0..20])
-- (dmap ((Just).succ) [0..20]) == (map succ [0..20])
-- gmap idAndPred [0..10]
-- gmap evenList [0..10]
--tst8 = [1,2,3] >>= pure.succ
--tst9 = [1,2,3] >>= \x -> [succ x]

test9 = [0..10] >>= evenList
test10 = [0..10] >>= idAndPred
