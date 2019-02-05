module FunctionManager where

data Record a = Record {entry::(String,a), callback::String->String} | None
data CallCenter a = CallCenter [(String,a)]


tst = CallCenter [("m2",(*2)),("m2",(+2)),("d1",(/2))]

ring (CallCenter ls) name = m

-- data FunctionScope a b = FunctionScope (Record (a->b)) (Record a) (Record b)
-- data FunctionDB a b = FunctionDB [FunctionScope a b]
--
-- -- variables, functions.
-- FunctionScope (Record ("mult 2",(*2))) (Record ("x",4)) (Record ("y",0))
--
-- access :: Record a b -> (String a)
-- access Record (name,a) cb = cb



-- instance Show (FunctionScope a b) where
--   show (FunctionScope (Record (s1,_)) (Record (s2,_)) (Record (s3,_))) = s1 ++ "("++s2++") = "++ s3
-- instance (Show a) => Show (Record a b) where
--   show (Record (str,v)) = str ++ ": "++ show v
-- instance Functor (Record a b) where
--   fmap fn (Record (s,val)) = Record (s, fn val)

-- FunctionScope (Record ("succ",succ)) (Record ("x",0))
--
-- fsApply :: FunctionScope a b -> b
-- fsApply (FunctionScope (_,fn) (_,val)) = fn val
-- (\x->x+4) <$> (Record ("Age",29))
