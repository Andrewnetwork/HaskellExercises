-- Experiments.hs
-- Andrew Ribeiro
-- December 2018

module Experiments where
import FunctionMachine
import Helpers

randFunc maxNum = makeDataset (\n->n*6) maxNum

-- Each function is a function of the machine state list
-- to a machine state. machine state list -> machine state.
--
-- Question
funM2 :: MachineInitter Int
funM2 = FuncMach [last,(+1).last, sum]

funM3 :: MachineInitter Int
funM3 = FuncMach [last,(\x->x-1).last, sum]

-- listPrograms (randFunc 20) funM2 4
-- listPrograms (randFunc 20) funM3 4
-- exeProgram' [2,2,0,2] (funM3 [2])


-- exeProgramOnLs [2,2,0,2] funM2 [0..20]

-- exeProgramOnLs [0,1,2] funM2 [0..20]
-- exeProgramOnLs [0,1,0,1,1] funM2 [0..20]
--randFunc maxNum = makeDataset (\n->(n+1)/3) maxNum
