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

tst342 = Just (5-) <*> Just 9

sequentalApplication :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
sequentalApplication a b c = a <$> b <*> c
-- sequentalApplication (++) ["0","1","2","3"] ["0,","1","2","3"]

sequentalApplication'
