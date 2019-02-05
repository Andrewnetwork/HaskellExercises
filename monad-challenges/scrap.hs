
-- [1,2,3] >>= (\x -> [x + 1])
customMap :: (t -> b) -> [t] -> [b]
customMap fn ls = ls >>= (\x -> [fn x])
-- customMap (\x -> x + 1) [1,2,3]
-- map (\x -> x + 1) [1,2,3]
-- concatMap (\x -> [x] ) [1,2,3]

customFilter :: (b -> Bool) -> [b] -> [b]
customFilter predicate ls = ls >>= (\x -> if predicate x then [x] else [])
-- customFilter (\x -> x < 3) [1,2,3,4,5]
