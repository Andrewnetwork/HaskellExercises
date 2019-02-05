import System.IO
import GHC.IO.Handle

--ioMain :: FilePath -> String -> IO String
-- ioMain file searchStr = do inh <- openFile file ReadMode
--                            handlePos <- mainLoop inh searchStr
--                            case handlePos of
--                              Nothing -> Nothing
--                              Just hp -> hpTest hp "test2.txt"
--                            hClose inh

hpTest :: FilePath -> FilePath -> String -> String -> IO String
hpTest sourceFile destFile searchStr insStr =
  do readHandler <- openFile sourceFile ReadMode
     mOffset <- mainLoop readHandler searchStr
     hClose readHandler
     rwHandler <- openFile destFile ReadWriteMode
     case mOffset of
       Nothing -> return "No value found."
       Just offset -> do hSetPosn handlePos
                         insertStr rwHandler ("\n"++insStr) ""
                         hClose rwHandler
                         return "All good."
                      where handlePos = (HandlePosn rwHandler (offset+1))

insertStr :: Handle -> String -> String -> IO ()
insertStr rwHandler (c:str) acc =
  do cAhead <- hLookAhead rwHandler
     hPutChar rwHandler c
     insertStr rwHandler str (acc ++ [cAhead])
insertStr rwHandler [] acc = hPutStrLn rwHandler ("\n"++acc)

-- hpTest "test1.txt" "test2.txt" "File" "F"
-- j = do m <- hpTest "test1.txt"
--        case m of
--          Nothing -> return "NONE"
--          Just x -> return "HEY"

mainLoop :: Handle -> String -> IO (Maybe Integer)
mainLoop handle searchStr =
  do ineof <- hIsEOF handle
     if ineof
       then return Nothing
       else do (HandlePosn _ hp) <- hGetPosn handle
               inpStr <- hGetLine handle
               if inpStr == searchStr
                 then return (Just hp)
                 else mainLoop handle searchStr

-- ioMain "test1.txt" "File"
