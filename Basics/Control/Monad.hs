-- monad.hs
-- Andrew Ribeiro
-- December 2018
module Monad where

data ThreeVal a 

instance Monad ThreeVal where
  (A a) >>= f = A (f a)
  (B b) >>= f = B (f b)
  (C c) >>= f = C (f c)
-- [(ThreeVal 1 2 3),(TreeVal 3.5 1.2 5.6)]
test =  [A 1,B 3.5, C "C", B 3.2, A 4, C "Hello"]
