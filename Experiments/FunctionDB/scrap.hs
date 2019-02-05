main :: String -> IO ()
main inFile = do
              inh <- openFile inFile ReadMode
              linePrinter inh 0
              hClose inh

linePrinter :: Handle -> Int -> IO ()
linePrinter inh counter =
    hIsEOF inh >>= (\ineof -> if ineof
                              then return ()
                              else printLine inh counter)
                              
printLine :: Handle -> Int -> IO ()
printLine inh counter = do inpStr <- hGetLine inh
                           putStrLn $ inpStr ++ " | " ++ show counter
                           linePrinter inh (counter+1)
