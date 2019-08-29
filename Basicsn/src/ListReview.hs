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
shiftCipher n = dictReplace (zip (shiftN lowerCases) lowerCases) . toLowerCase
    where shiftN = foldl1 (.) (replicate n shift)

-- >>> sequence (replicate 4 succ) $ 4
-- [5,5,5,5]
--
-- >>> :t foldl1 (.) (replicate 4 succ) 4
-- 8
--
-- >>> :t foldl1 (.)
-- foldl1 (.) :: Foldable t => t (a -> a) -> a -> a
--
