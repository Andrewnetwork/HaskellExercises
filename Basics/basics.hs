-- basics.hs
-- November 2018
-- Andrew Ribeiro

addThree :: Num a => a -> a -> a -> a
addThree a b c = a+b+c
-- addThree 1 2 3 -> 6
data FORA a b = F b | A a

-- The idea is to chain
--argumentCounter [] = A 0
--argumentCounter (A y) = A y
argumentCounter (F x) = x
-- argumentCounter addThree
