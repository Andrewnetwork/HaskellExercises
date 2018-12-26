--degenFunc :: Functor f => (a1 -> Maybe a2) -> f a1 -> f a2
--degenFunc fn f = filterFn $ fmap fn f

-- degenFunc evenMaybe [0,1..20]
-- filterFn $ map evenMaybe [0,1..20]
--  map f pixelData
--(<*>) :: f (a -> b) -> f a -> f b
--(<*>) = liftA2 id
-- fmap head [1,2,3]
-- head <*> [1,2,3]

tst5 :: Enum b => [b] -> [b]
tst5 = map succ

filterFn :: [Maybe a] -> [a]
filterFn ((Just x):xs) = x : filterFn xs
filterFn (Nothing:xs)  = filterFn xs
filterFn [] = []
