module Lists where
import Data.List hiding(unionBy)
import Data.Ord (comparing)
import Data.Function

-- test :: (a->a->Bool)->(a->a->a)->[a]->[a]->[a]
-- test 
unionBy                 :: (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy eq xs ys        =  xs ++ foldl (flip (deleteBy eq)) (nubBy eq ys) xs

-- >>> unionBy (==) [(10,5),(3,1),(2,3)] [(2,4),(10,3)]
-- [(2,3),(10,5),(3,1),(2,4),(10,3)]
--

-- Problem: Given a list of tuples....
-- op [(2,3),(10,5),(3,1)] [(2,4),(10,3)] = [(2,4),(10,5),(3,1)]
-- op [(2,3),(2,4)] [(2,6),(2,9)] = [(2,9)]
-- [(2,3),(2,4)] [(2,6),(2,9)] -> [(2,3),(2,4),(2,6),(2,9)]
-- 

maxValues :: (Ord a, Ord b) => [(a,b)]->[(a,b)]
maxValues = (maximumBy (comparing snd) <$>) . groupBy ((==) `on` fst) . sortOn fst

-- >>> sortOn fst [(3,1),(42,1),(1,1),(0,1),(10,1)]
-- [(0,1),(1,1),(3,1),(10,1),(42,1)]
--

-- >>> op3 [(2,3),(2,4),(2,6),(2,9)]
-- [(0,1),(1,1),(3,1),(10,1),(42,1)]
-- [(2,9)]
--
-- >>> group [2,2,3,1,3,4]
-- [[2,2],[3],[1],[3],[4]]
--

-- >>> [(2,3),(10,5),(2,4)] >>= \(a,b)->if a > 5 then return (a,b) else []
-- [(10,5)]
--
op ls1 ls2 = do 
    x<- ls1
    y<- ls2
    selectedValue x y

selectedValue x@(x1,x2) y@(y1,y2)
    | x1 == y1 = if x2 > y2 then return x else return y
    | otherwise = []

-- >>> op [(2,3),(2,4)] [(2,6),(2,9)]
-- [(2,6),(2,9),(2,6),(2,9)]
--

-- >>> op [(2,3),(10,5),(3,1)] [(2,4),(10,3)]
-- [(2,4),(10,5)]
--

-- >>> [(x1,y1) | (x1,x2) <- [(1,2),(3,4)], (y1,y2)<-[(1,6),(7,8)], x1 /= y1]
-- <interactive>:1280:2-28: error:
--     • Couldn't match expected type ‘[(Integer, Integer)] -> t’
--                   with actual type ‘[(Integer, Integer)]’
--     • The function ‘[(2, 3), (2, 4)]’ is applied to one argument,
--       but its type ‘[(Integer, Integer)]’ has none
--       In the expression: [(2, 3), (2, 4)] [(2, 6), (2, 9)]
--       In an equation for ‘it’: it = [(2, 3), (2, 4)] [(2, 6), (2, 9)]
--     • Relevant bindings include it :: t (bound at <interactive>:1280:2)
-- [(1,7),(3,1),(3,7)]
--