data E = Num Int | Var String | Soma E E | Mult E E
    deriving(Eq,Show)

type Memoria = [(String,Int)]


exSigma :: Memoria
exSigma = [ ("x", 10), ("temp",0), ("y",20) ]


procuraVar :: Memoria -> String -> Int
procuraVar [] v = error ("Variavel " ++ v ++ " nao encontrada")
procuraVar ((s,i):xs) v
    | s == v = i
    | otherwise = procuraVar xs v


smallStepE (Var x, s) = (Num (procuraVar s x), s)


smallStepE (Soma (Num n1) (Num n2), s) = (Num (n1 + n2), s)
smallStepE (Soma (Num n) e, s) = let (er,sr) = smallStepE (e,s) in (Soma (Num n) er, sr)
smallStepE (Soma e1 e2,s) = let (er,sr) = smallStepE (e1,s) in (Soma er e2,sr)

smallStepE (Mult (Num n1) (Num n2), s) = (Num (n1 * n2), s)
smallStepE (Mult (Num n) e, s) = let (er,sr) = smallStepE (e,s) in (Mult (Num n) er, sr)
smallStepE (Mult e1 e2,s) = let (er,sr) = smallStepE (e1,s) in (Mult er e2,sr)

prog1 :: E
prog1 = Soma (Num 1) (Mult (Var "x") (Var "y"))


isFinal :: E -> Bool
isFinal (Num n) = True
isFinal _ = False


interpretador :: (E,Memoria) -> (E, Memoria)
interpretador (e,s) = if (isFinal e) then (e,s) else interpretador (smallStepE (e,s))