
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

-- Space of natural numbers: [0,1..]
-- Space of multiples of two: map (*2) [0,1..]
-- take 100 $ map (*2) [0,1..]
-- take 100 $ map (*3) [0,1..]


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
