module Numbers where
import Data.Fixed (mod')

-- How Gauss originally thought about summing sequences of natural numbers.
-- Of course this is not as efficient as sum, but this setup leads to an algebraic abstraction.
gaussIntegerSum :: (Fractional p, Enum p) => p -> p
gaussIntegerSum end = (sum $ map (\(a,b) -> a+b) (zip ls (reverse ls))) / 2
                            where ls = [1..end]

gaussIntegerSumFormula end = (end*(end+1))/2

-- So gauss had some internal function that enabled him to do: gaussIntegerSum -> gaussIntegerSumFormula
-- Which we note is (sum $ map (\(a,b) -> a+b) (zip ls (reverse ls))) -> (end*(end+1))

collatz :: Int -> Int
collatz n
  | mod' n 2 == 0 = quot n 2
  | otherwise = 3*n + 1

collatzSeq n
  | n == 1 = [1]
  | otherwise = n:collatzSeq res
  where res = collatz n
-- btreeSort True $ collatzSeq $ 30
-- btreeSort False $ collatzSeq $ 30
-- btreeSort False $ collatzSeq $ 10000

-- listToTree $ collatzSeq 10
