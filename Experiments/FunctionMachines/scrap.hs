-- Example of "pure function" machine.
--
-- exeMachine (FuncMach [(\x->x+1),(\x->x+2)] 2) [0,1,0,1]
-- (\x->x+1).(\x->x+2).(\x->x+1).(\x->x+2) $ 2

exeMachine :: FunctionMachine a -> [Int] -> a
exeMachine (FuncMach procedures state) (pc:counters) = exeMachine (FuncMach procedures newState) counters
                                                         where newState = (procedures!!pc) state
exeMachine (FuncMach _ state) [] = state


-- fnMachine [(\x->x+1),(\x->x+2)] 2 [0,0,1,1] =
-- (fnMachine [(\x->x+1),(\x->x+2)] 2) <$> [[a,b,c,d] | a <- [0,1], b <- [0,1], c <- [0,1] , d <- [0,1] ]
-- state $ FuncMach [(\x->x+1),(\x->x+2)] 2

nCartesian :: Foldable t => t [a] -> [[a]]
nCartesian xss = foldr f [[]] xss
                 where f l a = [ x:xs | x <- l, xs <- a ]

fnMachine :: [t -> t] -> t -> [Int] -> t
fnMachine procedures state (p:procedurePointers) = fnMachine procedures ((procedures !! p) state) procedurePointers
fnMachine procedures state [] = state


-- Initial state resonance
data ISRFunctionMachine a = ISRFuncMach {procs::[a->a->a],initialState::a, s::a}


exeISR (ISRFuncMach procedures initialState state) (pc:counters) = exeISR (ISRFuncMach procedures initialState newState) counters
                                                                   where newState = (procedures !! pc) initialState state
exeISR (ISRFuncMach _ _ state) [] = state

-- exeMachine (FuncMach [(\x->x+1),(\x->x+2)] 2) [1,0,0]
-- targetFn : initalState -> terminalState
findPrograms :: Eq a => FunctionMachine a -> a -> Int -> [[Int]]
findPrograms fnMachine targetValue programLength = filter (\program->(exeMachine fnMachine program) == targetValue) programSpace
                                                   where programSpace = nCartesian $ replicate programLength [0..(length $ procedures fnMachine)-1]

findISR :: Eq a => ISRFunctionMachine a -> a -> Int -> [[Int]]
findISR isrMach targetValue programLength = filter (\program->(exeISR isrMach program) == targetValue) programSpace
                                            where programSpace = nCartesian $ replicate programLength [0..(length $ procs isrMach)-1]


-- findPrograms (FuncMach [(\x->x+1),(\x->x+2)] 2) 8 3
-- findPrograms (FuncMach [id,(\x->x+1),(\x->x+2)] 2) 4 3
-- findPrograms (FuncMach [(\x->x+1),(\x->x+2)] 2) 4 1

-- Problem: Design a function machine that can learn: n -> n*(n+1)/2
-- findPrograms (FuncMach [id,(\x->x+1),(\x->x*x),(\x->x/2)] 3) 6 4




-- Machine Initializers
funcM1 = FuncMach [id,(\x->x+1),(\x->x*x),(\x->x/2),(\x->x-1),(\x->x**2),(\x->(-x))]
funcM2 s = ISRFuncMach [(\i x->x),(\i x-> i+x),(\i x->x*(i+1)),(\i x->x*(i+2)),
                        (\i x->i*x),(\i x->x/2),(\i x->x/4),(\i x->x/8),(\i x->x/3)] s s
--[3,1,8,7,3,2]
-- (\i x->x*(i+2)).(\i x-> i+x).(\i x->x/3).(\i x->x/8).(\i x->x*(i+2).(\i x->x*(i+1))


-- intersect (findISR (funcM2 1.0) 1.0 4) (findISR (funcM2 2.0) 3.0 4)
-- exeISR = exeISR funcM2 [0,0,0]
-- intersect (findPrograms (funcM1 1.0) 1.0 4) (findPrograms (funcM1 2.0) 3.0 4)
-- intersect [[6,6,5,4]] (findPrograms (funcM1 2.0) 3.0 4)
-- (1->1) (2->3)
--
listPrograms :: Eq b => [(a, b)] -> (a -> ISRFunctionMachine b) -> Int -> [[[Int]]]
listPrograms ((i,o):xs) isrMach progLen = progs : listPrograms xs isrMach progLen
                                         where progs = findISR (isrMach i) o progLen
listPrograms [] _ _ = []

fullListPrograms lstProgs = foldl1 intersect lstProgs

exeISRProgram :: [Int] -> (a -> ISRFunctionMachine b) -> [a] -> [b]
exeISRProgram program machineInitter inputs = (flip $ exeISR) program <$> (map machineInitter inputs)

--fullListPrograms $ listPrograms [(1.0,1.0),(2.0,3.0)] funcM2 4
--fullListPrograms $ listPrograms sumOfNatLs funcM2 3
--fullListPrograms $ listPrograms sumOfSumOfNatLs funcM2 6
--fullListPrograms $ listPrograms sumOfSumOfNatLs funcM2 2
-- [3,1,8,7,3,2]
-- fullListPrograms $ listPrograms sumOfNatLs funcM2 2
-- fullListPrograms $ listPrograms sumOfSumOfNatLs funcM2 6
-- (\i x->x*(i+1)).(\i x->x/2)
-- map (\m-> exeISR m [3,1,8,7,3,2] ) (map funcM2 [0..10])
-- exeISRProgram [3,1,8,7,3,2] funcM2 [0..10]
-- exeISRProgram [3,1,2,7,3,8] funcM2 [0..20]
--  fullListPrograms $ listPrograms (sumOfSumOfNatLs 3) funcM2 6
-- exeISRProgram [0,0,4,4,1,5] funcM2 [0..10]
-- fullListPrograms $ listPrograms (sumOfSumOfNatLs 4) funcM2 6
-- exeISRProgram [3,1,2,3,7,8] funcM2 [0..10]
-- (sumOfSumOfNatLs 10)

-- (\x->(head x)+(last x)
-- (+) <$> head <*> last  $ [1,2,3]
-- (*) <$> last <*> ((1+) . head)
