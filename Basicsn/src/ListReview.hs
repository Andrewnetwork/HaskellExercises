{-|
Module: ListReview
Description: A review of Data.List
Copyright: (c) Andrew Ribeiro 
Maintainer: andrewnetwork@gmail.com 
-}
module ListReview where
import           Data.List
import           Data.Tuple
import           Data.Foldable
------------------------------------------------------------------------
-------------------------- Basic Functions -----------------------------
------------------------------------------------------------------------

-- >>> "Hello" ++ " " ++ "world!"
-- "Hello world!"
--
-- >>> "Hello " ++ ['w','o','r','l','d','!']
-- "Hello world!"
--
-- >>> firstAndLast "Hello world!"
-- "H!"
--
firstAndLast ls = [head ls, last ls]

-- >>> tail "Hello world!"
-- "ello world!"
--
-- >>> init "Hello world!"
-- "Hello world"
--
-- >>> uncons "Hello world!"
-- Just ('H',"ello world!")
--
-- >>> null []
-- True
--
-- >>> null "Hello"
-- False
--
-- >>> null ""
-- True
--
-- >>> length "Hello world!"
-- 12
--

------------------------------------------------------------------------
------------------------ List Transformations --------------------------
------------------------------------------------------------------------

-- >>> shift "ABC"
-- "BCA"
--
shift (x : xs) = xs ++ [x]

lowerCases = "abcdefghijklmnopqrstuvwxyz"
upperCases = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
letterDict = zip lowerCases upperCases

dictReplace :: Eq a => [(a, a)] -> [a] -> [a]
dictReplace dict (x : xs) = case x `lookup` dict of
    Just c  -> c : dictReplace dict xs
    Nothing -> x : dictReplace dict xs
dictReplace _ [] = []

-- >>> toUpperCase "hello world!"
-- "HELLO WORLD!"
--
-- >>> toLowerCase "HELLO WORLD!" 
-- "hello world!"
--
toUpperCase = dictReplace letterDict
toLowerCase = dictReplace (map swap letterDict)

-- >>> shiftCipher 12 "Andrew Ribeiro"
-- "obrfsk fwpswfc"
--
shiftCipher :: Int -> String -> String
shiftCipher n = dictReplace (zip (shiftN lowerCases) lowerCases) . toLowerCase
    where shiftN = foldl1 (.) (replicate n shift)

-- >>> reverse "Hello World!"
-- "!dlroW olleH"
--
-- >>> intersperse ' ' "Hello world!" 
-- "H e l l o   w o r l d !"
--
-- >>> subsequences [1,2,3,4] 
-- [[],[1],[2],[1,2],[3],[1,3],[2,3],[1,2,3],[4],[1,4],[2,4],[1,2,4],[3,4],[1,3,4],[2,3,4],[1,2,3,4]]
--
-- >>> permutations [1,2,3]
-- [[1,2,3],[2,1,3],[3,2,1],[2,3,1],[3,1,2],[1,3,2]]
--
-- >>> or [False,False,False]
-- False
--
-- >>> or [False,True,False]
-- True
--
-- >>> scanl (+) 0 [5,4,3,2,1,2,3,4,5]
-- [0,5,9,12,14,15,17,20,24,29]
--
-- >>> mapAccumL (\a b -> (succ a, b+a)) 0 [1,2,3,4]
-- (4,[1,3,5,7])
--
-- >>> maxVal [1,45,3,2,1,3]
-- 45
--
maxVal :: [Integer] ->  Integer
maxVal = foldl1 (\a b -> if a > b then a else b)