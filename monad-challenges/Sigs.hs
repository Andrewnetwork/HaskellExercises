-- Type signatures for Set 1 and Set 2.

-- Set 1
type Gen a = Seed -> (a, Seed)
randLetter :: Gen Char
generalA :: (a->b) -> Gen a -> Gen b
randEven :: Gen Integer
randPair :: Gen (Char, Integer)
generalPair :: Gen a -> Gen b -> Gen (a,b)
generalB :: (a->b->c) -> Gen a -> Gen b -> Gen c
repRandom :: [Gen a] -> Gen [a]
genTwo :: Gen a -> (a -> Gen b) -> Gen b
mkGen :: a -> Gen a

-- Set 2
data Maybe a = Just a | Nothing deriving(Eq)
headMay :: [a] -> Maybe a
lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
queryGreek :: GreekData -> String -> Maybe Double
chain :: (a -> Maybe b) -> Maybe a -> Maybe b
link :: Maybe a -> (a -> Maybe b) -> Maybe b
addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
mkMaybe :: a -> Maybe a
combine :: Maybe (Maybe a) -> Maybe a
yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c

-- Equivalences
yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
generalB :: (a->b->c) -> Gen a -> Gen b -> Gen c

genTwo :: Gen a -> (a -> Gen b) -> Gen b
link :: Maybe a -> (a -> Maybe b) -> Maybe b
