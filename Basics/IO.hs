module IO where
import System.Console.ANSI
import Data.List (intercalate)

printRed :: String -> IO ()
printRed str = do setSGR [SetColor Foreground Vivid Red]
                  putStr str
                  setSGR [Reset]

printGreen :: String -> IO ()
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
--  printEvenOddList $ collatzSeq $ 30000

gridString :: (Show a) => [[a]] -> String
gridString  []   = "┌─┐\n└─┘\n"
gridString  [[]] = "┌─┐\n└─┘\n"
gridString  xxs  = (++ bot) $ concat $ zipWith (\a b -> unlines [a, b]) (top : replicate rowC mid) rows
  where
    rowC   = pred . length $ xxs
    colC   = pred . length . head $ xxs
    top    = "┌" ++ repC "─┬" ++ "─┐"
    mid    = "├" ++ repC "─┼" ++ "─┤"
    bot    = "└" ++ repC "─┴" ++ "─┘"
    repC  = concat . replicate colC
    rows   = (++ "|") . ('|' :) . intercalate "|" . ((\x -> show x) <$>) <$> xxs
