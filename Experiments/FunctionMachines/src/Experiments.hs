-- Experiments.hs
-- Andrew Ribeiro
-- December 2018

module Experiments where
-- import FunctionMachine
import Helpers



-- Each function is a function of the machine state list
-- to a machine state. machine state list -> machine state.
--
-- ########### Machine Initializers ######################
funM1 :: MachineInitter Double Double
funM1 = FuncMach
  [ last
  , liftA2 (+) head last
  , liftA2 (*) last ((+1) . head)
  , liftA2 (*) last ((+2) . head)
  , liftA2 (*) last head
  , (/2) . last
  , (/3) . last
  , (/4) . last
  , (/8) . last
  ]

funM2 :: MachineInitter Int Int
funM2 = FuncMach
  [ last
  , (+1) . last
  , sum
  ]

funM3 :: MachineInitter Int Int
funM3 = FuncMach
  [ last
  , pred . last
  , sum
  ]

-- | will this work
-- >>> listPrograms (randFunc 20) funM2 4
-- [[0,0,2,2],[0,2,0,2],[2,0,2,2],[2,2,0,2]]
--
-- >>> listPrograms (randFunc 20) funM3 4
-- [[0,0,2,2],[0,2,0,2],[2,0,2,2],[2,2,0,2]]
--
-- >>> exeProgram' [2,2,0,2] (funM3 [2])
-- [2,2,4,4,12]
--
-- >>> exeProgramOnLs [2,2,0,2] funM2 [0..20]
-- [0,6,12,18,24,30,36,42,48,54,60,66,72,78,84,90,96,102,108,114,120]

-- >>> exeProgramOnLs [0,1,2] funM2 [0..20]
-- [1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49,52,55,58,61]
-- >>> exeProgramOnLs [0,1,0,1,1] funM2 [0..20]
-- [3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]
--randFunc maxNum = makeDataset (\n->(n+1)/3) maxNum
