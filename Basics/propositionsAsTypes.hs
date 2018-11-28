-- propositionsAsTypes.hs
-- Andrew Ribeiro
-- November 2018

-- Examples of logical propositions. 
-- If it rains (r) it pours (p). r -> p. 
implies x y = not x || y
p1 r p = implies r p

-- Boolean logic can be seen as a sort of filtering algebra 
-- for tuples of booleans.
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
-- cartProd [True, False] [True, False]
-- cartProd [True, False] (cartProd [True, False] [True, False])

--booleanTuples :: Int -> [Bool] -> [(Bool,Bool)] 
--booleanTuples n ls = booleanTuples (n-1) (cartProd [True,False] ls)
--booleanTuples 0 ls = ls