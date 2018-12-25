-- Helpers.hs
-- Andrew Ribeiro
-- December 2018

module Helpers where

nCartesian :: Foldable t => t [a] -> [[a]]
nCartesian xss = foldr f [[]] xss
                 where f l a = [ x:xs | x <- l, xs <- a ]

randFunc :: (Num b, Enum b) => b -> [(b, b)]
randFunc maxNum = makeDataset (*6) maxNum

-- ####### Datasets ###########
makeDataset :: (Num a, Enum a) => (a -> b) -> a -> [(a, b)]
makeDataset fn maxNum = zip [0..maxNum] $ fn <$> [0..maxNum]

sumOfNat :: Fractional a => a -> a
sumOfNat n  = (n * (n + 1)) / 2

sumOfNatLs :: (Enum b, Fractional b) => b -> [(b, b)]
sumOfNatLs maxNum = makeDataset sumOfNat maxNum

sumOfSumOfNat :: Fractional a => a -> a
sumOfSumOfNat n = (n * (n + 1) *(n + 2) * (n + 3)) / 24

sumOfSumOfNatLs :: (Enum b, Fractional b) => b -> [(b, b)]
sumOfSumOfNatLs maxNum = makeDataset sumOfSumOfNat maxNum
