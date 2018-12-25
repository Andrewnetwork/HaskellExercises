-- FunctionMachine.hs
-- Andrew Ribeiro
-- December 2018

module FunctionMachine where

import Data.List
import Control.Monad
import Control.Applicative
import Helpers

data FunctionMachine a = FuncMach
                       { procedures :: [[a] -> a]
                       , state      :: [a]
                       } deriving (Show)

type Program             = [Int]
type MachineInitter a  b = [a] -> FunctionMachine b


-- | what does this do?
-- Examples:
--
-- >>> exeProgram' [0,1,0,1] (FuncMach [(\x->(last x)+1),(\x->(last x)+2)] [2])
-- [2,3,5,6,8]
--
-- >>> exeProgram' [0] (FuncMach [(\x->(last x)+1),(\x->(last x)+2)] [2])
-- [2,3]
--
exeProgram' :: Program -> FunctionMachine a -> [a]
exeProgram' counters (FuncMach procedures state) = foldl (\s c -> s ++ [procedures !! c $ s]) state counters

-- | what does this do?
-- Examples:
--
-- >>> exeProgram [0,1,0,1] (FuncMach [(\x->(last x)+1),(\x->(last x)+2)] [2])
-- 8
--
-- >>> exeProgram [0] (FuncMach [(\x->(last x)+1),(\x->(last x)+2)] [2])
-- 3
--
exeProgram :: Program -> FunctionMachine a -> a
exeProgram = (last .) . exeProgram'

-- ########## Program Search ######
-- | explain function
--
-- Examples:
--
-- >>> findPrograms (funM1 [1]) 8 4
-- [[0,1,2,2],[0,2,2,2],[0,3,1,2],[1,0,2,2],[1,1,1,2],[1,2,0,2],[1,2,2,0],[1,2,2,4],[1,2,4,2],[1,3,1,1],[1,4,2,2],[2,0,2,2],[2,1,1,2],[2,2,0,2],[2,2,2,0],[2,2,2,4],[2,2,4,2],[2,3,1,1],[2,4,2,2],[3,0,1,2],[3,1,0,2],[3,1,2,0],[3,1,2,4],[3,1,4,2],[3,2,1,1],[3,4,1,2],[4,1,2,2],[4,2,2,2],[4,3,1,2],[6,1,2,3],[6,1,3,2]]
--
findPrograms :: Eq a => FunctionMachine a -> a -> Int -> [Program]
findPrograms funMach targetValue programLength =
  filter ((==targetValue) . (flip exeProgram funMach)) programSpace
    where programSpace = replicateM programLength [0..(length $ procedures funMach) - 1]

-- | explain fucntion
--
-- Examples:
-- >>> listPrograms' (sumOfNatLs 10) funM1 2
-- [[[0,0],[0,1],[0,2],[0,3],[0,4],[0,5],[0,6],[0,7],[0,8],[1,0],[1,1],[1,2],[1,3],[1,4],[1,5],[1,6],[1,7],[1,8],[2,0],[2,1],[2,2],[2,3],[2,4],[2,5],[2,6],[2,7],[2,8],[3,0],[3,1],[3,2],[3,3],[3,4],[3,5],[3,6],[3,7],[3,8],[4,0],[4,1],[4,2],[4,3],[4,4],[4,5],[4,6],[4,7],[4,8],[5,0],[5,1],[5,2],[5,3],[5,4],[5,5],[5,6],[5,7],[5,8],[6,0],[6,1],[6,2],[6,3],[6,4],[6,5],[6,6],[6,7],[6,8],[7,0],[7,1],[7,2],[7,3],[7,4],[7,5],[7,6],[7,7],[7,8],[8,0],[8,1],[8,2],[8,3],[8,4],[8,5],[8,6],[8,7],[8,8]],[[0,0],[0,4],[1,5],[2,5],[3,6],[4,0],[4,4],[5,2],[6,3]],[[2,5],[5,1],[5,2]],[[0,1],[1,0],[2,5],[5,2]],[[2,5],[5,2]],[[1,1],[2,5],[5,2]],[[2,5],[5,2]],[[2,5],[5,2]],[[2,5],[5,2]],[[2,5],[5,2]],[[2,5],[5,2]]]
--
listPrograms' :: Eq b => [(a, b)] -> MachineInitter a b -> Int -> [[Program]]
listPrograms' ios funMach programLength = (\(i,o) -> findPrograms (funMach [i]) o programLength) <$> ios

-- | explain function
--
-- Examples:
--
-- >>> listPrograms (sumOfNatLs 10) funM1 2
-- [[2,5],[5,2]]
--
listPrograms :: Eq b => [(a, b)] -> MachineInitter a b -> Int -> [Program]
listPrograms ios funMach programLength = foldl1 intersect $ listPrograms' ios funMach programLength

-- | describe function
-- Examples:
--
-- >>> exeProgramOnLs [5,2] funM1 [0..20]
-- [0.0,1.0,3.0,6.0,10.0,15.0,21.0,28.0,36.0,45.0,55.0,66.0,78.0,91.0,105.0,120.0,136.0,153.0,171.0,190.0,210.0]
--
-- >>> exeProgramOnLs [2,5] funM1 [0..10]
-- [0.0,1.0,3.0,6.0,10.0,15.0,21.0,28.0,36.0,45.0,55.0]
--
exeProgramOnLs :: Program -> MachineInitter a b  -> [a] -> [b]
exeProgramOnLs program machineInitter inputs = exeProgram program . machineInitter . pure <$> inputs

-- >>> listPrograms (sumOfSumOfNatLs 10) funM1 6
-- [[3,1,2,3,6,8],[3,1,2,3,8,6],[3,1,2,6,3,8],[3,1,2,6,8,3],[3,1,2,8,3,6],[3,1,2,8,6,3],[3,1,3,2,6,8],[3,1,3,2,8,6],[3,1,3,6,2,8],[3,1,3,6,8,2],[3,1,3,8,2,6],[3,1,3,8,6,2],[3,1,6,2,3,8],[3,1,6,2,8,3],[3,1,6,3,2,8],[3,1,6,3,8,2],[3,1,6,8,2,3],[3,1,6,8,3,2],[3,1,8,2,3,6],[3,1,8,2,6,3],[3,1,8,3,2,6],[3,1,8,3,6,2],[3,1,8,6,2,3],[3,1,8,6,3,2]]
--
-- map (\program-> exeProgram program (funM1 [1])) (findPrograms' (funM1 [1]) 8 4)
-- replicateM 3 [0,1,2]
-- listPrograms (randFunc 10) funM1 2
