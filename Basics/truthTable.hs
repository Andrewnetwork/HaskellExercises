-- truthTable.hs
-- credit: matheusdev23
{-# LANGUAGE FlexibleInstances #-}
module Table where

chompInput :: ([Bool], Bool -> b) -> [([Bool], b)]
chompInput (inputsBefore, outputSoFar) =
    [(True:inputsBefore, outputSoFar True), (False:inputsBefore, outputSoFar False)]
 
evalBool :: [([Bool], Bool -> b)] -> [([Bool], b)]  
evalBool inputs = concatMap chompInput inputs
 
class Table a where
    table :: [([Bool], a)] -> [([Bool], Bool)]
 
instance Table Bool where
    table = id
 
instance Table a => Table (Bool -> a) where
    table = table . evalBool
 
wrap func = [([], func)]
 
test = table (wrap (&&))
 
test2 = table (wrap not)