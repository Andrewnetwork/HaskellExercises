-- CapacityContext.hs
-- Andrew Ribeiro
-- February 2019

module CapacityContext where

data Capacity a = Capacity a Int | None

instance (Show a) => Show (Capacity a) where
  show (Capacity a cap) = show a ++ " | " ++ show cap ++ " left."
  show None = "End of Capacity"

instance Functor Capacity where
  --fmap :: Functor f => (a -> b) -> f a -> f b
  fmap fn (Capacity a cap)
    | cap > 1 = Capacity (fn a) (cap-1)
    | otherwise = None

instance Applicative Capacity where
  pure a = Capacity a 0
  -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
  (<*>) (Capacity fn fnCap) (Capacity val valCap) = Capacity (fn val) (fnCap+valCap-1)
  (<*>) None _ = None
  (<*>) _ None = None


main capacity = do putStrLn "== Begin Transaction =="
                   putStrLn (show capacity)
                   modifier <- getLine
                   let modInt  = (read modifier :: Integer)
                   let res = ( (\x->x+modInt) <$> capacity)
                   case res of (Capacity _ _) -> main res
                               None -> putStrLn "Capacity Exhausted."

-- (Capacity (\x->x+4) 4) <*> (Capacity 3 2)

tst :: Capacity Integer
tst = Capacity 0 5
-- main tst

-- Capacity (+100) 5 <*> Capacity 0 5
