-- FunctionMachine.hs
-- Andrew Ribeiro
-- December 2018

module FunctionMachine where
import Data.List
import Control.Monad
import Helpers

data FunctionMachine a = FuncMach { procedures::[[a]->a], state::[a] }
type Program = [Int]

exeProgram' :: Program -> FunctionMachine a -> [a]
exeProgram' (pc:counters) (FuncMach procedures state) = exeProgram' counters (FuncMach procedures newState)
                                                        where newState = state++[(procedures!!pc) state]
exeProgram' [] (FuncMach _ state) = state

exeProgram :: Program -> FunctionMachine a -> a
exeProgram program fnMachine = last $ exeProgram' program fnMachine

-- exeProgram' [0,1,0,1] (FuncMach [(\x->(last x)+1),(\x->(last x)+2)] [2])
-- exeProgram' [0] (FuncMach [(\x->(last x)+1),(\x->(last x)+2)] [2])

-- ########### Machine Initializers ######################
funM1 :: [Double] -> FunctionMachine Double
funM1 = FuncMach [last,\x->head x + last x,\x->last x * (head x + 1),\x->last x * (head x + 2),
                   \x->head x * last x,(/2).last,(/3).last,(/4).last,(/8).last]

-- ########## Program Search ######
findPrograms :: Eq a => FunctionMachine a -> a -> Int -> [Program]
findPrograms funMach targetValue programLength = filter (\program->(exeProgram program funMach) == targetValue) programSpace
                                                 where programSpace = replicateM programLength [0..(length $ procedures funMach)-1]

listPrograms' :: Eq b => [(a, b)] -> ([a] -> FunctionMachine b) -> Int -> [[Program]]
listPrograms' ((i,o):xs) funMach programLength = progs : listPrograms' xs funMach programLength
                                               where progs = findPrograms ( funMach [i] ) o programLength
listPrograms' [] _ _ = []

listPrograms :: Eq b => [(a, b)] -> ([a] -> FunctionMachine b) -> Int -> [Program]
listPrograms ((i,o):xs) funMach programLength = foldl1 intersect lstProgs
                                                where lstProgs = listPrograms' ((i,o):xs) funMach programLength

exeProgramOnLs :: Program -> ([a] -> FunctionMachine b) -> [a] -> [b]
exeProgramOnLs program machineInitter inputs = exeProgram program <$> machineInitter.pure <$> inputs

-- listPrograms (sumOfNatLs 10) funM1 2
-- listPrograms (sumOfSumOfNatLs 10) funM1 6
-- findPrograms' (funM1 [1]) 8 4
-- map (\program-> exeProgram program (funM1 [1])) (findPrograms' (funM1 [1]) 8 4)
-- exeProgramOnLs [5,2] funM1 [0..20]
-- exeProgramOnLs [2,5] funM1 [0..10]
-- replicateM 3 [0,1,2]
