import Control.Applicative

-- Why is it infered that the type of tst is exclusively defined
-- for integers, not Num in general?

tst = liftA2 (+) head last

tst2 = (+)

tst3 a b = 3.5 + a + b
