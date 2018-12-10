module IO where
import System.Console.ANSI

printRed str = do setSGR [SetColor Foreground Vivid Red]
                  putStr str
                  setSGR [Reset]
printGreen str = do setSGR [SetColor Foreground Vivid Green]
                    putStr str
                    setSGR [Reset]
tst = printGreen "Hey Boi" `mappend` printRed " Hey\n"

--printEvenOddList :: (Integral a) => [a] -> IO
printEvenOddList [] = putStr "\n"
printEvenOddList (x:xs)
  | mod x 2 == 0 = printGreen printString `mappend` printEvenOddList xs
  | otherwise = printRed printString `mappend` printEvenOddList xs
  where printString = show x ++ " "

-- printEvenOddList $ concatMap collatzSeq [1..1000000000]
-- printEvenOddList [1..1000000000]
