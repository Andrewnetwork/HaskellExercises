-- propsAndTypes.hs 
-- November 2018 
-- Andrew Ribeiro 
-- ****
-- Here I will explore the relationship between propositions and types. 
-- ****

-- Example 1: Typed Ordered Pairs 
-- Here I show directly how to encode the result of a predicate operation as the construction of a type. 
data OP = E Int Int | L Int Int | G Int Int deriving Show

-- Constructs a typed ordered pair. 
makeOP :: Int->Int->OP
makeOP a b
    | a > b = G a b
    | a < b = L a b
    | a == b = E a b

-- A pattern matching labeling function. 
label :: OP -> [Char]
label (E a b) = "Equal to"
label (L a b) = "Less than"
label (G a b) = "Greater than"
-- label (makeOP 1 2)
-- label (makeOP 2 1)
-- label (makeOP 2 2)

-- Sorting a list of ordered pairs.
-- [(E 1 1),(E 1 1)] 
-- [(E 1 1),(E 1 1),(G 2 1)] 
-- [(E 1 1),(E 1 1),(E 2 2)] 




