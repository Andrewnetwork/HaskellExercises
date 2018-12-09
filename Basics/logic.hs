-- logic.hs
-- Andrew Ribeiro
-- November 2018

-- Logical connectives
-- and: a && b
-- or:  a || b
-- implication: not x || y

-- Logical unary connective
-- not: not x

-- Examples of logical propositions.
-- If it rains (r) it pours (p). r -> p.
implies :: Bool -> Bool -> Bool
implies x y = not x || y
-- True `implies` True -> True

-- Boolean logic can be seen as a sort of filtering algebra
-- for tuples of booleans.
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
-- cartProd [True, False] [True, False]
-- cartProd [True, False] (cartProd [True, False] [True, False])

--booleanTuples :: Int -> [Bool] -> [(Bool,Bool)]
--booleanTuples n ls = booleanTuples (n-1) (cartProd [True,False] ls)
--booleanTuples 0 ls = ls

applyToTuple :: (a -> t1 -> t2) -> (a, t1) -> t2
applyToTuple binaryFn tup = binaryFn (fst tup) (snd tup)

connectiveTT :: (Bool -> Bool -> Bool) -> [((Bool, Bool), Bool)]
connectiveTT connective = zip boolTups (map (applyToTuple connective) boolTups)
                          where boolTups = (cartProd [True,False] [True,False])
-- connectiveTT implies -> [((True,True),True),((True,False),False),((False,True),True),((False,False),True)]


p1 :: Bool -> Bool -> Bool -> Bool
p1 a b c = not(a && b) || c

p2 a b c d = not(a && b) || (c || d)
-- connectiveTT (p1 True)
-- p1 True True False
-- connectiveTT (p1 True) ++ connectiveTT (p1 False)
-- connectiveTT (p1 True) ++ connectiveTT (p1 False)

--map connectiveTT (p1 True)
-- credit: matheusdev23
modTup :: a -> ((b1, c), b2) -> ((a, b1, c), b2)
modTup x ((b1, b2), b3) = ((x, b1, b2), b3)

appendTuple :: a -> [((b1, c), b2)] -> [((a, b1, c), b2)]
appendTuple x tupLs = map (modTup x) tupLs

-- (appendTuple True (connectiveTT (p1 True))) ++ (appendTuple False (connectiveTT (p1 False)))
--

--truthTable' prop = connectiveTT (prop True)

-- credit: matheusdev23
chompInput :: ([Bool], Bool -> b) -> [([Bool], b)]
chompInput (inputsBefore, outputSoFar) =
    [(True:inputsBefore, outputSoFar True), (False:inputsBefore, outputSoFar False)]

evalBool inputs = concatMap chompInput inputs

test = evalBool (evalBool [([], (&&))])
-- evalBool (evalBool (evalBool (evalBool [([], p2)])) )
