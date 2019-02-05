-- FileLineData.hs
-- Andrew Ribeiro
-- February 2019
module FileLineData where
import System.IO
import Data.List.Split
import Data.Char (isSpace)
import Test.DocTest
import Text.Read (readMaybe)

test = doctest ["FileLineData.hs"]

data LineData = LineData String LineDataType | LineDataError String | None
data LineDataType = LDS String | LDI Int | LDD Double

instance Show LineData where
  show (LineData name val) = name ++ " : " ++ show val
  show None = "No data."
  show (LineDataError errorMsg) = "ERROR: "++errorMsg
instance Show LineDataType where
  show (LDS str) = "String" ++ " : " ++ str
  show (LDI int) =  "Int" ++ " : " ++ show int
  show (LDD double) = "Double" ++ " : " ++ show double

trim :: (Char -> Bool) -> String -> String
trim predicate  = f . f
                  where f = reverse . dropWhile predicate
-- | trim
-- >>> trim (== ' ') "   Dude    Hello Buddy   Guy     "
-- "Dude    Hello Buddy   Guy"
-- >>> concatMap (trim isSpace) (splitOn " " "   Dude    Hello Buddy   Guy     ")
-- "DudeHelloBuddyGuy"

parseLD :: [String]-> LineData
parseLD [nameS,typeS,valS]
  | typeS == "Int"    = case readMaybe valS :: Maybe Int of
                          Nothing  -> LineDataError "Could not parse as Int."
                          Just x   -> LineData nameS (LDI x)
  | typeS == "Double" = case readMaybe valS :: Maybe Double of
                          Nothing  -> LineDataError "Could not parse as Double."
                          Just x   -> LineData nameS (LDD x)
  | typeS == "String" = LineData nameS (LDS valS)
  | otherwise = LineDataError "Not a valid type."
-- | parseLD
-- >>> parseLD ["name","String","Andrew"]
-- name : String : Andrew
-- >>> parseLD ["name","Stoing","Andrew"]
-- ERROR: Not a valid type.
-- >>> parseLD ["name","Double","!"]
-- ERROR: Could not parse as Double.

parseLine :: String -> LineData
parseLine str = case map (trim isSpace) (splitOn ":" str) of
                  args@[_,_,_] -> parseLD args
                  _ -> LineDataError "Does not conform to the 'id : type : value' syntax."
-- | parseLine
-- >>> parseLine "test : Int : 23"
-- "test" : Int : 23
-- >>> parseLine "log : String : Hello World"
-- "log" : String : "Hello World"
-- >>> parseLine "gpa : Double : 3.4"
-- "gpa" : Double : 3.4
-- >>> parseLine "sdjfka"
-- ERROR: Does not conform to the 'id : type : value' syntax.
-- >>> parseLine "test : KF : KF KF"
-- ERROR: Not a valid type.
-- >>> parseLine "test : Double : !"
-- ERROR: Could not parse as Double.
-- >>> parseLine "test : !"
-- ERROR: Does not conform to the 'id : type : value' syntax.
-- >>> parseLine "ds"
-- ERROR: Does not conform to the 'id : type : value' syntax.

readLoop :: FilePath -> String -> IO LineData
readLoop inFile key = do inh <- openFile (inFile++".ld") ReadMode
                         (readLoop' inh key) <* hClose inh

readLoop' :: Handle -> String -> IO LineData
readLoop' inh key =
 do ineof <- hIsEOF inh
    if ineof
        then return None
        else do inpStr <- hGetLine inh
                case parseLine inpStr of
                  res@(LineData name _ ) -> if name == key then return res else readLoop' inh key
                  x -> readLoop' inh key

writeLoop :: FilePath -> String -> LineDataType -> IO ()
writeLoop file key value = do inh <- openFile (file++".ld") ReadMode
                              outh <- openFile (file++".change.ld") WriteMode
                              writeLoop' inh outh key value
                              hClose inh
                              hClose outh

writeLoop' inh outh key value =
    do ineof <- hIsEOF inh
       if ineof
           then return ()
           else do inpStr <- hGetLine inh
                   case parseLine inpStr of
                     (LineData name _) -> if name == key
                                            then hPutStrLn outh (show (LineData name value))
                                            else writeLoop' inh outh key value
-- writeLoop "values" "log" (LDS "TEST 1")
-- writeLoop "values" "test" (LDI 34)

--




-- fn key fn = mainL "values.ld" key >>= (\ld -> if fn ld == ld then ld else  )
-- readLoop "values.ld" "log" >>= (\(LineData name ldt)-> putStrLn.show $ ldt)
(<@>) :: (LineDataType -> LineDataType) -> String -> IO LineData
(<@>) fn key = readLoop "values" key >>= (\ld -> return $ route ld)
               where route v = case v of
                                (LineData name ldt) -> LineData name (fn ldt)
                                x -> x
-- | (<@>)
-- >>> id <@> "log"
-- No data.
-- >>> id <@> "age"
-- age : Int : 29
-- >>> id <@> "welcomingMessage"
-- welcomingMessage : String : Hello mate!
-- >>> (\(LDS x) -> LDS $ x++"!!!!!") <@> "welcomingMessage"
-- welcomingMessage : String : Hello mate!!!!!!
-- >>> id <@> "gpa"
