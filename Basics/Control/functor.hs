-- functor.hs
-- Andrew Ribeiro
-- December 2018
import Data.Functor
import Control.Applicative

data List a = LI a (List a) | None  deriving Show

instance Functor List where
  fmap f (LI a tailLs) = LI (f a) (fmap f tailLs)
  fmap f None = None


-- [(\x->x+100)] <*> ([(\x->x+5)] <*> [1,2,3])
-- (selfComposeLs 5 (\x->x+1)) <*> [0]
-- (replicate 4 (\x->x+1)) <*> [1]
selfComposeLs n fn = selfComposeLs' n fn id
selfComposeLs' n fn afn
  | n > 0 = (afn.fn) : (selfComposeLs' (n-1) fn (afn.fn))
  | otherwise = []

-- length $ selfComposeLs 3 (\x->x+3)
-- Showing this obeys the functor laws.
-- Identity: (fmap id (LI 3 (LI 4 None ))) == (LI 3 (LI 4 None ))
-- Composablity: fmap ((\x->x**3).(\x->x+2)) (LI 3 (LI 4 None )) ==
--               fmap (\x->x**3) (fmap (\x->x+2) (LI 3 (LI 4 None )))
--               AKA: (\x->x**3) <$> (\x->x+2) <$> (LI 3 (LI 4 None ))
--               AKA: (\x->x**3).(\x->x+2) <$> (LI 3 (LI 4 None ))
--
-- Experiments:
-- (\x->x**3) <$> (LI 3 (LI 4 None ))
-- (\x->x**3) <$> (\x->x+2) <$> (LI 3 (LI 4 None ))
-- (LI 3 (LI 4 None )) <&> (\x->x**3)
-- liftA2 f x y = f <$> x <*> y
-- (<*>) = liftA2 id
-- liftA2 f x y = f <$> x <*> y

--test :: Maybe List
--test = pure (LI 3 (LI 4 None ))
-- pure 1 :: Maybe Int

-- [(\x->x*2),(\x->x*3)] <*> [1,2,3,4]
-- [(\x->x*3).(\x->x*3)] <*> [1,2,3,4]
-- liftA2 (\x y->(x,y)) [0,1,2] [0,1,2]
-- liftA (\x->x**3) [0,1,2]
-- liftA2 (,) [0,1,2] [0,1,2]
-- [1,2] *> [4,5,6]
--

test :: [Int] -> (Int->[Int]) -> [Int]
test ls fn = ls >>= fn

--  ["apple", "car", "banana"] >>= (\x -> [x ++ "hello"])
-- [1,2,3] >>= (\x->[x*2])
