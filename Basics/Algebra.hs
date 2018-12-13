data OPType = M | D | S | A deriving Show
data OP a = OPC OPType (OP a) (OP a) | Arg (Entity a)  deriving Show
data Entity a = Var Int | Const a deriving Show
data Part a = OPP (OP a) | EP (Entity a) deriving Show
data Equation a = EQN (Part a) (Part a)  deriving Show


-- EQ ( M (Const 2) (Const 3) ) (Var 1)
-- M (LArg Const 2) (RArg Const 3)
-- EQN (OPP (M (Arg (Const 3)) (Arg (Const 3)))) (EP (Var 1))

--solveEquation :: Equation a -> [(Entity a,a)]
--solveEquation (EQN (Part a) (Part b)) = case a

applyOP :: Fractional a => OP a -> a
applyOP (OPC M (Arg (Const a)) (Arg (Const b))) = a * b
applyOP (OPC D (Arg (Const a)) (Arg (Const b))) = a / b
applyOP (OPC S (Arg (Const a)) (Arg (Const b))) = a - b
applyOP (OPC A (Arg (Const a)) (Arg (Const b))) = a + b

-- applyOP (M (Arg (Const 3)) (Arg (Const 3)))
makeOP :: OPType -> Entity a -> Entity a -> OP a
makeOP opType a b = (OPC opType (Arg a) (Arg  b))
--makeOP M (Var 1) (Const 4)

makeEQN l r = EQN l r
-- makeEQN (makeOP M (Var 1) (Const 4)) (EP (Const 8))
