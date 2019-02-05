{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude
import Test.DocTest

test = doctest ["Set2.hs"]

-- ## Problem 1 ##
data Maybe a = Just a | Nothing deriving(Eq)

instance Show a => Show (Maybe a) where
  show (Just a) = "Just " ++ show a
  show Nothing  = "Nothing"

-- ## Problem 2 ##
headMay :: [a] -> Maybe a
headMay (a:as) = Just a
headMay []     = Nothing
-- | headMay -- safe version of Prelude.head
-- >>> headMay [1,2]
-- Just 1
-- >>> headMay []
-- Nothing

tailMay :: [a] -> Maybe [a]
tailMay (a:as) = Just as
tailMay []     = Nothing
-- | tailMay -- safe version of Prelude.tail
-- >>> tailMay [1,2,3,4]
-- Just [2,3,4]
-- >>> tailMay []
-- Nothing

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay key dict = case filterRes of [] -> Nothing
                                       ((_,b):_) -> Just b
                     where filterRes = filter (\(a,_)->a==key) dict
-- | lookupMay
-- >>> lookupMay 1 [(1,2),(3,4),(1,5)]
-- Just 2
-- >>> lookupMay 1 []
-- Nothing

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay a 0 = Nothing
divMay a b = Just (a/b)
-- | divMay
-- >>> divMay 60 2
-- Just 30.0
-- >>> divMay 20 0
-- Nothing

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay ls = Just (foldr1 max ls)
-- | maximumMay
-- >>> maximumMay [1,2,3,4,3]
-- Just 4
-- >>> maximumMay []
-- Nothing

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay ls = Just (foldr1 min ls)
-- | minimumMay
-- >>> minimumMay [1,2,3,4,3]
-- Just 1
-- >>> minimumMay []
-- Nothing

-- ## Problem 3 ##
queryGreek :: GreekData -> String -> Maybe Double
queryGreek greekData key =
  case lookupMay key greekData of
    Nothing  -> Nothing
    Just jxs -> case tailMay jxs of
                  Nothing -> Nothing
                  Just t  -> case maximumMay t of
                                Nothing -> Nothing
                                Just mt -> case headMay jxs of
                                              Nothing      -> Nothing
                                              Just headRes -> divMay (fromIntegral mt) (fromIntegral headRes)
-- | queryGreek
-- >>> queryGreek greekDataA "alpha"
-- Just 2.0
-- >>> queryGreek greekDataA "beta"
-- Nothing
-- >>> queryGreek greekDataA "gamma"
-- Just 3.3333333333333335
-- >>> queryGreek greekDataA "delta"
-- Nothing
-- >>> queryGreek greekDataA "zeta"
-- Nothing
-- >>> queryGreek greekDataB "rho"
-- Nothing
-- >>> queryGreek greekDataB "phi"
-- Just 0.24528301886792453
-- >>> queryGreek greekDataB "chi"
-- Just 9.095238095238095
-- >>> queryGreek greekDataB "psi"
-- Nothing
-- >>> queryGreek greekDataB "omega"
-- Just 24.0

-- ## Problem 4 ##
chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain _ Nothing          = Nothing
chain maybeFn (Just val) = maybeFn val
-- | chain
-- >>> chain headMay (tailMay [1,2,3])
-- Just 2
-- >>> chain headMay (tailMay [2,4,3])
-- Just 4
-- >>> headMay `chain` (tailMay [2,4,3])
-- Just 4

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link Nothing _          = Nothing
link (Just val) maybeFn = maybeFn val
-- | link
-- >>> link (tailMay [1,2,3]) headMay
-- Just 2
-- >>> (tailMay [1,2,3]) `link` headMay
-- Just 2
-- >>> Nothing `link` headMay
-- Nothing
-- >>> (link (tailMay [1,2,3]) headMay) == (flip chain (tailMay [1,2,3]) headMay)
-- True

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 greekData key = lookupMay key greekData `link` (\a ->
                              tailMay a `link` (\b ->
                                maximumMay b `link` (\c ->
                                  headMay a `link` (\d -> divMay (fromIntegral c) (fromIntegral d)))))

-- | queryGreek2
-- >>> queryGreek2 greekDataA "alpha"
-- Just 2.0
-- >>> queryGreek2 greekDataA "beta"
-- Nothing
-- >>> queryGreek2 greekDataA "gamma"
-- Just 3.3333333333333335
-- >>> queryGreek2 greekDataA "delta"
-- Nothing
-- >>> queryGreek2 greekDataA "zeta"
-- Nothing
-- >>> queryGreek2 greekDataB "rho"
-- Nothing
-- >>> queryGreek2 greekDataB "phi"
-- Just 0.24528301886792453
-- >>> queryGreek2 greekDataB "chi"
-- Just 9.095238095238095
-- >>> queryGreek2 greekDataB "psi"
-- Nothing
-- >>> queryGreek2 greekDataB "omega"
-- Just 24.0

-- headMay [1,2,3] `link` (\a -> Just (succ a))

-- ## Problem 5 ##
mkMaybe :: a -> Maybe a
mkMaybe a = Just a
-- | mkMaybe
-- >>> mkMaybe 1
-- Just 1
-- >>> mkMaybe 2.3
-- Just 2.3

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries sals n1 n2 = lookupMay n1 sals `link` (\s1->lookupMay n2 sals `link` (\s2-> mkMaybe (s1+s2) ))
-- | addSalaries
-- >>> addSalaries salaries "carol" "bob"
-- Just 175000
-- >>> addSalaries salaries "carol" "matt"
-- Nothing

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink fn m1 m2 =  m1 `link` (\a-> m2 `link` (\b -> mkMaybe (fn a b)))
-- | yLink
-- >>> yLink (+) (lookupMay "carol" salaries) (lookupMay "matt" salaries)
-- Nothing
-- >>> yLink (+) (lookupMay "carol" salaries) (lookupMay "bob" salaries)
-- Just 175000

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 sals n1 n2 = yLink (+) (lookupMay n1 sals) (lookupMay n2 sals)
-- | addSalaries2
-- >>> addSalaries2 salaries "carol" "bob"
-- Just 175000
-- >>> addSalaries salaries "carol" "matt"
-- Nothing

-- ## Problem 6 ##
tailProd :: Num a => [a] -> Maybe a
tailProd = transMaybe product . tailMay
-- | tailProd
-- >>> tailProd []
-- Nothing
-- >>> tailProd [3]
-- Just 1
-- >>> tailProd [1,2,3]
-- Just 6

tailSum :: Num a => [a] -> Maybe a
tailSum = transMaybe sum . tailMay
-- | tailSum
-- >>> tailSum []
-- Nothing
-- >>> tailSum [5]
-- Just 0
-- >>> tailSum [1,2,3]
-- Just 5

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe fn (Just a) = Just (fn a)
transMaybe _ Nothing = Nothing
-- | transMaybe
-- >>> transMaybe sum (Just [1,2,3])
-- Just 6
-- >>> transMaybe sum Nothing
-- Nothing

tailMin :: Ord a => [a] -> Maybe a
tailMin = combine . transMaybe minimumMay . tailMay
-- | tailMin
-- >>> tailMin [1,2,3]
-- Just 2
-- >>> tailMin []
-- Nothing

tailMax :: Ord a => [a] -> Maybe a
tailMax = combine . transMaybe maximumMay . tailMay
-- | tailMax
-- >>> tailMax [1,2,3]
-- Just 3
-- >>> tailMax []
-- Nothing

combine :: Maybe (Maybe a) -> Maybe a
combine (Just a) = a
combine Nothing = Nothing
-- | combine
-- >>> combine Nothing
-- Nothing
-- >>> combine (Just . Just $ 2)
-- Just 2
