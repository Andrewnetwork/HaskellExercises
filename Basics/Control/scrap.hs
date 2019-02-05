--degenFunc :: Functor f => (a1 -> Maybe a2) -> f a1 -> f a2
--degenFunc fn f = filterFn $ fmap fn f

-- degenFunc evenMaybe [0,1..20]
-- filterFn $ map evenMaybe [0,1..20]
--  map f pixelData
--(<*>) :: f (a -> b) -> f a -> f b
--(<*>) = liftA2 id
-- fmap head [1,2,3]
-- head <*> [1,2,3]


{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

data BinaryCounterTree a = None | Tree a Int (BinaryCounterTree a) (BinaryCounterTree a) deriving (Show, Functor)


tst5 :: Enum b => [b] -> [b]
tst5 = map succ

filterFn :: [Maybe a] -> [a]
filterFn ((Just x):xs) = x : filterFn xs
filterFn (Nothing:xs)  = filterFn xs
filterFn [] = []

test9 = [0..10] >>= evenList
test10 = [0..10] >>= idAndPred
-- [(\x->x+2)] <*> [1,2,3,4]
--  [(\x->x+2)] <*> [(\x->x+2)] <*> [1,2,3,4]

data GlobalContext a b c = GlobalContext (a->b->c) a

instance Show a => Show (GlobalContext a b c) where
  show (GlobalContext fn global) = show global

tst34 = GlobalContext (+) 3

(<@>) (GlobalContext fn global) arg  =  GlobalContext fn (fn global arg)

tst345 = tst34 <@> 3 <@> 4
