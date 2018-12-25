module Main where

import Lib
import FunctionMachine
-- import Helpers
import Test.DocTest

main :: IO ()
main = doctest ["FunctionMachine.hs", "Experiments.hs"]
