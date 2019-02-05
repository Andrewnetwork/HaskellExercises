module DebugTrace where

import Test.DocTest

test = doctest ["DebugTrace.hs"]

addOne = (+) 1
-- | addOne
-- >>> addOne 3
-- 4

addTwoAndOne a b = addOne $ a + b
-- | addTwoAndOne
-- >>> addTwoAndOne 3 2
-- 6

printLog :: [Char] -> IO ()
printLog str = putStrLn $ reverse (drop 1 (reverse str))

logger str = (++ "\n") . (str ++) . show
-- | logger
-- >>> logger "" $ addOne 3
-- "4\n"
-- >>> printLog $ logger (logger "" $ addOne 3) $ addOne 5
-- 4
-- 6
-- >>> printLog $ logger (logger "" $ addTwoAndOne 3 5) $ addTwoAndOne 1 2
-- 9
-- 4
-- >>> printLog $ logger (logger (logger "" $ addTwoAndOne 3 5) $ addTwoAndOne 1 2) $ addOne 2
-- 9
-- 4
-- 3
-- >>> mapM_ putStrLn $ map show [addTwoAndOne 3 5 , addTwoAndOne 1 2, addOne 2]
-- 9
-- 4
-- 3
