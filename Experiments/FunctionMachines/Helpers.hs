-- Helpers.hs
-- Andrew Ribeiro
-- December 2018

module Helpers where

nCartesian :: Foldable t => t [a] -> [[a]]
nCartesian xss = foldr f [[]] xss
                 where f l a = [ x:xs | x <- l, xs <- a ]

-- ####### Datasets ###########
makeDataset :: (Num a, Enum a) => (a -> b) -> a -> [(a, b)]
makeDataset fn maxNum = zip [0..maxNum] $ fn <$> [0..maxNum]

sumOfNat n = (n*(n+1))/2
sumOfNatLs maxNum = makeDataset sumOfNat maxNum

sumOfSumOfNat n = (n*(n+1)*(n+2)*(n+3))/24
sumOfSumOfNatLs maxNum = makeDataset sumOfSumOfNat maxNum
