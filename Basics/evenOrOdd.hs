-- evenOrOdd.hs
-- Andrew Ribeiro
-- November 2018

-- First we will make some functions that takes in a number 
-- and produces a list of numbers. We will then introduce the use
-- of types to ensure that a list is a list of even, odd, or mixed
-- numbers. This is mostly an exercise in haskelling, not a practical
-- application. 

naturalNumbers :: Int -> [Int]
naturalNumbers n
    | n == 0 = [0]
    | otherwise = naturalNumbers (n-1) ++ [n]
-- naturalNumbers 3 -> [0,1,2,3]

evenNumbers :: Int -> [Int]
evenNumbers n = [x | x <- naturalNumbers n , (mod x 2) == 0]
-- evenNumbers 10 -> [0,2,4,6,8,10]

oddNumbers :: Int -> [Int]
oddNumbers n = [x | x <- naturalNumbers n , (mod x 2) /= 0]
-- oddNumbers 10 -> [1,3,5,7,9]

-- Now we will introduce a datatype for even,odd, and even or odd numbers. 
-- Note: we are not using dependent types, so we can construct absurd instances like (E 3). 
data Even = E Int deriving Show
data Odd  = O Int deriving Show
data EvenOrOdd = EE Int | OO Int deriving Show

numType :: Int -> EvenOrOdd
numType x
    | (mod x 2) == 0 = EE x 
    | otherwise = OO x
-- numType 3 -> (OO 3)
-- numType 4 -> (EE 4)

typeNumLs :: [Int] -> [EvenOrOdd]
typeNumLs ls = map numType ls
-- typeNumLs (evenNumbers 10) -> [EE 0,EE 2,EE 4,EE 6,EE 8,EE 10]
-- typeNumLs (evenNumbers 10) :: [EvenOrOdd Int]

-- This function produces a list of even numbers from a list of even 
-- or odd numbers if the type system allows it. 
evenResolve :: EvenOrOdd -> Even
evenResolve (EE x) = E x
evenResolve (OO x) = error ( (show x) ++ " is not an even number!" )
-- evenResolve (EE 4) -> (E 4)
-- evenResolve (OO 3) -> Error: "3 is not an even number!"

oddResolve :: EvenOrOdd -> Odd
oddResolve (OO x) = O x
oddResolve (EE x) = error ( (show x) ++ " is not an odd number!" )

-- Now, from a list ls, we construct a list of even numbers if the type system allows us. 

constructEvenList :: [Int] -> [Even]
constructEvenList ls = map evenResolve (typeNumLs ls)
-- constructEvenList (evenNumbers 10) -> [E 0,E 2,E 4,E 6,E 8,E 10]
-- constructEvenList (oddNumbers  10) -> Error: "1 is not an even number!"
-- constructEvenList [2,3,4]  -> Error: "3 is not an even number!"

constructOddList :: [Int] -> [Odd]
constructOddList ls = map oddResolve (typeNumLs ls)
-- constructOddList (oddNumbers  10) -> [O 1,O 3,O 5,O 7,O 9]
-- constructOddList (evenNumbers 10) -> Error: "0 is not an odd number!"

-- In this file we've established the following transformations. 
-- Int -> EvenOrOdd
-- [Int] -> [EvenOrOdd] -> [Odd]
-- [Int] -> [EvenOrOdd] -> [Even]
-- [Int] -> [Odd]
-- [Int] -> [Even]