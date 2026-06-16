data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E
      | Igual E E  
   deriving(Eq,Show)

data C = While B C
    | If B C C
    | Seq C C
    | Atrib E E
    | Skip
    | TenTimes C   
    | Repeat C B 
    | Loop E E C      
    | DuplaAtrib E E E E 
    | AtribCond B E E E 
    | Swap E E 
   deriving(Eq,Show)         



type Memoria = [(String,Int)]

exSigma :: Memoria
exSigma = [ ("x", 10), ("temp",0), ("y",0)]



procuraVar :: Memoria -> String -> Int
procuraVar [] s = error ("VariavreturnE " ++ s ++ " nao definida no estado")
procuraVar ((s,i):xs) v
  | s == v     = i
  | otherwise  = procuraVar xs v



mudaVar :: Memoria -> String -> Int -> Memoria
mudaVar [] v n = error ("VariavreturnE " ++ v ++ " nao definida no estado")
mudaVar ((s,i):xs) v n
  | s == v     = ((s,n):xs)
  | otherwise  = (s,i): mudaVar xs v n




smallStepE :: (E, Memoria) -> (E, Memoria)

smallStepE (Var x, sigma) = (Num (procuraVar sigma x), sigma)

smallStepE (Soma (Num n1) (Num n2), sigma) = (Num (n1 + n2), sigma)
smallStepE (Soma (Num n) e, sigma) = let (evalE,evalSigma) = smallStepE (e,sigma) in (Soma (Num n) evalE, evalSigma)
smallStepE (Soma e1 e2,sigma) = let (evalE, evalSigma) = smallStepE (e1,sigma) in (Soma evalE e2,evalSigma)

smallStepE (Mult (Num n1) (Num n2), sigma) = (Num (n1 * n2), sigma)
smallStepE (Mult (Num n) e, sigma) = let (evalE,evalSigma) = smallStepE (e,sigma) in (Mult (Num n) evalE, evalSigma)
smallStepE (Mult e1 e2,sigma) = let (evalE,evalSigma) = smallStepE (e1,sigma) in (Mult evalE e2,evalSigma)

smallStepE (Sub (Num n1) (Num n2), sigma) = (Num (n1 - n2), sigma)
smallStepE (Sub (Num n) e, sigma) = let (evalE, evalSigma) = smallStepE (e,sigma) in (Sub (Num n) evalE, evalSigma)
smallStepE (Sub e1 e2,sigma) = let (evalE, evalSigma) = smallStepE (e1,sigma) in (Sub evalE e2, evalSigma)


smallStepB :: (B,Memoria) -> (B, Memoria)

smallStepB (Not FALSE, sigma) = (TRUE, sigma)
smallStepB (Not TRUE, sigma) = (FALSE, sigma) 
smallStepB (Not b, sigma) = let(evalB, evalSigma) = smallStepB( b , sigma ) in (Not evalB, evalSigma) 

smallStepB (And TRUE b, sigma ) = ( b , sigma )
smallStepB (And FALSE b , sigma) = ( FALSE, sigma )
smallStepB (And b1 b2, sigma ) = let (evalB, evalSigma) = smallStepB(b1 , sigma) in (And evalB b2 , evalSigma)


smallStepB (Or TRUE b, sigma ) = ( TRUE , sigma )
smallStepB (Or FALSE b , sigma) = ( b, sigma )
smallStepB (Or b1 b2, sigma ) = let (evalB, evalSigma) = smallStepB(b1 , sigma) in (Or evalB b2 , evalSigma)

smallStepB (Leq (Num n1) (Num n2) , sigma) 
   | n1 <= n2 = ( TRUE , sigma )
   | otherwise = ( FALSE , sigma)
smallStepB (Leq (Num n) e, sigma) = let(evalE , evalSigma) = smallStepE(e, sigma) in (Leq (Num n) evalE, evalSigma)
smallStepB (Leq e1 e2, sigma) = let(evalE , evalSigma) = smallStepE(e1, sigma) in (Leq evalE e2, evalSigma)

smallStepB (Igual (Num n1) (Num n2) , sigma) 
   | n1 == n2 = ( TRUE , sigma )
   | otherwise = ( FALSE , sigma)
smallStepB (Igual (Num n) e, sigma) = let(evalE , evalSigma) = smallStepE(e, sigma) in (Igual (Num n) evalE, evalSigma)
smallStepB (Igual e1 e2, sigma) = let(evalE , evalSigma) = smallStepE(e1, sigma) in (Igual evalE e2, evalSigma)

smallStepC :: (C,Memoria) -> (C,Memoria)



smallStepC (If TRUE c1 c2, sigma) = (c1, sigma)
smallStepC (If FALSE c1 c2, sigma) = (c2, sigma)
smallStepC (If b c1 c2, sigma) = let(evalB, evalSigma) = smallStepB (b, sigma) in (If evalB c1 c2, evalSigma)  

smallStepC (Seq Skip c2, sigma) = (c2, sigma)
smallStepC (Seq c1 c2, sigma) = let(evalC, evalSigma) = smallStepC(c1, sigma) in (Seq evalC c2, evalSigma)

smallStepC (Atrib (Var var) (Num n), sigma) = (Skip, (mudaVar sigma var n))
smallStepC (Atrib (Var var) e, sigma) = let (evalE, evalSigma) = smallStepE(e, sigma) in (Atrib (Var var) evalE, evalSigma)

smallStepC (While b c, sigma) = (If b (Seq c (While b c)) (Skip), sigma)

smallStepC (TenTimes c,sigma) = ( Seq c ( Seq c ( Seq c ( Seq c ( Seq c ( Seq c ( Seq c ( Seq c ( Seq c ( Seq c Skip ) ) ) ) ) ) ) ) ),sigma )


smallStepC (Repeat c b, sigma) = (If (Not b) (Seq c (Repeat c b)) c, sigma)   

smallStepC (Loop e1 e2 c, sigma) = (If (Leq (Soma e1 (Num 1) ) e2) (Seq c ( Loop e1 (Sub e2 (Num 1)) c ) ) Skip,sigma )

smallStepC (DuplaAtrib (Var var1) (Var var2) e1 e2, sigma) = (Seq (Atrib (Var var1) e1 ) (Atrib (Var var2) e2 ),sigma )

smallStepC (AtribCond b (Var var) e1 e2, sigma) = (If b (Atrib (Var var) e1) (Atrib (Var var) e2), sigma)

smallStepC (Swap (Var var1) (Var var2), sigma) = 
   let (n1, sigma1) = smallStepE( (Var var1),sigma ) 
       (n2, sigma2) = smallStepE( (Var var2),sigma ) 
   in smallStepC(DuplaAtrib (Var var1) (Var var2) n2 n1 , sigma )



isFinalE (Num n) = True
isFinalE _       = False


interpretadorE :: (E,Memoria) -> (E, Memoria)
interpretadorE (e,s) = if (isFinalE e) then (e,s) else interpretadorE (smallStepE (e,s))


isFinalB :: B -> Bool
isFinalB TRUE    = True
isFinalB FALSE   = True
isFinalB _       = False

interpretadorB :: (B,Memoria) -> (B, Memoria)
interpretadorB (b,s) = if (isFinalB b) then (b,s) else interpretadorB (smallStepB (b,s))


isFinalC :: C -> Bool
isFinalC Skip    = True
isFinalC _       = False

interpretadorC :: (C,Memoria) -> (C, Memoria)
interpretadorC (c,s) = if (isFinalC c) then (c,s) else interpretadorC (smallStepC (c,s))



fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))

mySigma :: Memoria
mySigma = [("x",3), ("y",2), ("z",0)]


testSub :: (E, Memoria)
testSub = interpretadorE ( ( Sub ( Var "x" ) ( Var "y" ) ) , mySigma )
testNotT :: (B, Memoria)
testNotT = interpretadorB( (Not TRUE) , mySigma )
testNotF :: (B, Memoria)
testNotF = interpretadorB( (Not FALSE) , mySigma )
testNotAnd :: (B, Memoria)
testNotAnd = interpretadorB( (Not ( And TRUE TRUE ) ) , mySigma )
testAndT = interpretadorB( ( And (Not FALSE) (Not FALSE) ) , mySigma )
testAndF = interpretadorB( ( And (Not FALSE) (Not TRUE) ) , mySigma )
testOrT = interpretadorB( ( Or (Not FALSE) (Not FALSE) ) , mySigma )
testOrF = interpretadorB( ( Or FALSE (Not TRUE) ) , mySigma )
testLeqT = interpretadorB( ( Leq ( Soma (Num 1) (Num 2) ) ( Soma (Num 4) (Num 2) ) ) , mySigma )
testLeqF = interpretadorB( ( Leq ( Soma (Num 1) (Num 2) ) ( Soma (Num 1) (Num 1) ) ) , mySigma )
testIgualT = interpretadorB( ( Igual ( Soma (Num 1) (Num 1) ) ( Soma (Num 1) (Num 1) ) ) , mySigma )
testIgualF = interpretadorB( ( Igual ( Soma (Num 2) (Num 1) ) ( Soma (Num 1) (Num 1) ) ) , mySigma )


fatorialMem :: Memoria
fatorialMem = [("x",6), ("y",0)]

programFatorial :: (C, Memoria)
programFatorial = interpretadorC(fatorial, fatorialMem)



sigmaSumToN :: Memoria
sigmaSumToN = [("s",1),("n",5), ("sum",0)]

sumToN :: C
sumToN = Loop ( Num 1 ) ( Soma (Var "n") (Num 1) )
            ( Seq (Atrib (Var "sum") (Soma (Var "s") (Var "sum")))
               (Atrib (Var "s") (Soma (Var "s") (Num 1))))

programSumToN :: (C,Memoria)
programSumToN = interpretadorC(sumToN, sigmaSumToN)



sigmaFibonaci :: Memoria
sigmaFibonaci = [("lastlast",0),("last",0),("n", 1),("step", 0),("steps",8)]

seqFibonaci :: C
seqFibonaci = Seq (DuplaAtrib ( Var "lastlast" ) (Var "last") ( Var "last") ( Var "n"))
                  ( Atrib (Var "n") (Soma (Var "last") (Var "lastlast")))

fibonaci :: C
fibonaci = Repeat (Seq seqFibonaci (Atrib (Var "step")
               (Soma (Var "step") (Num 1))))
                  (Leq (Var "steps") (Var "step") )

programFibonaci :: (C,Memoria)
programFibonaci = interpretadorC(fibonaci, sigmaFibonaci)


sigmaMax :: Memoria
sigmaMax = [("x",20),("y",10),("max",0)]

maxMe :: C
maxMe = AtribCond (Leq (Var "x") (Var "y")) (Var "max") (Var "y") (Var "x")

maxProgram :: (C,Memoria)
maxProgram = interpretadorC(maxMe, sigmaMax)

sigmaSwap :: Memoria
sigmaSwap = [("x",20),("y",10)]

swap :: C
swap = (Swap (Var "x") (Var "y"))

programSwap :: (C,Memoria)
programSwap = interpretadorC( swap , sigmaSwap )