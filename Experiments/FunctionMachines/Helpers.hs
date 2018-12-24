module Helpers where

nCartesian :: Foldable t => t [a] -> [[a]]
nCartesian xss = foldr f [[]] xss
                 where f l a = [ x:xs | x <- l, xs <- a ]

sumOfNat n = (n*(n+1))/2
sumOfNatLs maxNum = zip [0.0..maxNum] (map sumOfNat [0..maxNum])
sumOfSumOfNat n = (n*(n+1)*(n+2)*(n+3))/24
sumOfSumOfNatLs maxNum = zip [0.0..maxNum] (map sumOfSumOfNat [0..maxNum])
