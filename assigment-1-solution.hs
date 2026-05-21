data E = Num Int
      |Var String
      |VarDin E -- Just for Bubble Sort 
      |Soma E E
      |Sub E E
      |Mult E E
      |Div E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or B B
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
    | DuplaATrib E E E E 
    | AtribCond B E E E 
    | Swap E E 
   deriving(Eq,Show)                


type Memoria = [(String,Int)]

exSigma :: Memoria
exSigma = [ ("x", 10), ("temp",0), ("y",0)]

procuraVar :: Memoria -> String -> Int
procuraVar [] searchVar = error ("Variavel " ++ searchVar ++ " nao definida no estado")
procuraVar ((var,value):xs) searchVar
  | var == searchVar = value
  | otherwise  = procuraVar xs searchVar

mudaVar :: Memoria -> String -> Int -> Memoria
mudaVar [] searchVar num = error ("Variavel " ++ searchVar ++ " nao definida no estado")
mudaVar ((var,value):xs) searchVar num
  | var == searchVar = ((var,num):xs)
  | otherwise = (var,value): mudaVar xs searchVar num


ebigStep :: (E,Memoria) -> Int

ebigStep (Var var,mem) = procuraVar mem var

ebigStep (Num num,mem) = num

ebigStep (Soma e1 e2,mem) = ebigStep (e1,mem) + ebigStep (e2,mem) -- Using External Operations

ebigStep (Sub e1 e2,mem) = ebigStep (e1,mem) - ebigStep (e2,mem) -- Using External Operations

ebigStep (Mult e1 e2,mem) = ebigStep (e1,mem) * ebigStep (e2,mem) -- Using External Operations

ebigStep (Div e1 e2,mem) = ebigStep (e1,mem) `div` ebigStep (e2,mem) -- Using External Operations

ebigStep (VarDin e , mem) = procuraVar mem (show (ebigStep (e, mem)))


bbigStep :: (B,Memoria) -> Bool

bbigStep (TRUE,mem) = True

bbigStep (FALSE,mem) = False

bbigStep (Not b,mem) 
   | bbigStep (b,mem) == True = False
   | otherwise = True 

bbigStep (And b1 b2,mem )
   | bbigStep (b1,mem) == True = bbigStep (b2,mem)
   | otherwise = False

bbigStep (Or b1 b2,mem )
   | bbigStep (b1,mem) == True = True
   | otherwise = bbigStep (b2,mem)

bbigStep (Leq e1 e2,mem) = ebigStep (e1,mem) <= ebigStep (e2,mem) -- Using External Operations

bbigStep (Igual e1 e2,mem) = ebigStep (e1,mem) == ebigStep (e2,mem) -- Using External Operations



cbigStep :: (C,Memoria) -> (C,Memoria)

cbigStep (Skip,mem) = (Skip,mem)

cbigStep (If b c1 c2,mem)
   | bbigStep (b,mem) = cbigStep (c1,mem)
   | otherwise = cbigStep (c2,mem) 

cbigStep (Seq c1 c2,mem) = cbigStep(c2, (snd (cbigStep(c1,mem))))  

cbigStep (Atrib (Var var) e,mem) = (Skip, mudaVar mem var (ebigStep (e,mem)))

cbigStep (While b c,mem)
   | bbigStep (b,mem) = cbigStep (Seq c (While b c),mem)
   | otherwise = (Skip,mem)

cbigStep (TenTimes c,mem) = cbigStep (Seq c (Seq c (Seq c (Seq c (Seq c (Seq c (Seq c (Seq c (Seq c Skip)))))))), mem)

cbigStep (Repeat c b,mem)
   | bbigStep (b,mem) == False = cbigStep(Seq c (Repeat c b),mem) 
   | otherwise = cbigStep(c,mem)

cbigStep (Loop e1 e2 c,mem)
   | bbigStep( Leq e2 e1, mem ) = cbigStep(Skip,mem)
   | bbigStep( Igual (Sub e2 e1 ) (Num 0), mem ) = cbigStep(Skip,mem) 
   | otherwise = cbigStep( Seq c ( Loop e1 ( Sub e2 (Num 1) ) c ) , mem)

cbigStep (AtribCond b (Var var) e2 e3 , mem)
   | bbigStep (b,mem) = cbigStep (Atrib (Var var) e2,mem)
   | otherwise = cbigStep (Atrib (Var var) e3,mem)

cbigStep (DuplaATrib (Var var1) (Var var2) e1 e2, mem) = cbigStep (Seq (Atrib (Var var1) e1) (Atrib (Var var2) e2), mem)


cbigStep ( Swap (Var var1) (Var var2) , mem ) = cbigStep( DuplaATrib (Var var1) (Var var2) (Num ( ebigStep((Var var2) , mem))) (Num ( ebigStep((Var var1) , mem))), mem)

cbigStep(Swap (VarDin e1) (VarDin e2), mem) =
      let v1 = show (ebigStep (e1, mem))
          v2 = show (ebigStep (e2, mem))
      in cbigStep (DuplaATrib (Var v1) (Var v2)
                    (Num (procuraVar mem v2))
                    (Num (procuraVar mem v1)), mem)

exSigma2 :: Memoria
exSigma2 = [("x",5), ("y",0), ("z",0)]


progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))


teste1 :: B
teste1 = (Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3)))

teste2 :: B
teste2 = (Leq (Soma (Var "x") (Num 3))  (Mult (Num 2) (Num 3)))


testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
               (Atrib (Var "y") (Var "z")))

fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))


sigmaSumToN :: Memoria
sigmaSumToN = [("s",1),("n",5), ("sum",0)]

sumToN :: C
sumToN = (Loop ( Num 1 ) ( Soma (Var "n") (Num 1) ) 
            ( Seq (Atrib (Var "sum") (Soma (Var "s") (Var "sum"))) 
               (Atrib (Var "s") (Soma (Var "s") (Num 1)))))

programSumToN :: (C,Memoria)
programSumToN = cbigStep(sumToN, sigmaSumToN)

sigmaFibonaci :: Memoria
sigmaFibonaci = [("lastlast",0),("last",0),("n", 1),("step", 0),("steps",8)]

seqFibonaci :: C
seqFibonaci = (Seq (DuplaATrib ( Var "lastlast" ) (Var "last") ( Var "last") ( Var "n")))
                  ( Atrib (Var "n") (Soma (Var "last") (Var "lastlast")))

fibonaci :: C
fibonaci = ( Repeat (Seq (seqFibonaci) (Atrib (Var "step") 
               (Soma (Var "step") (Num 1))))
                  (Leq (Var "steps") (Var "step") ) )

programFibonaci :: (C,Memoria)
programFibonaci = cbigStep(fibonaci, sigmaFibonaci)




sigmaBubbleSort1 :: Memoria
sigmaBubbleSort1 = [("size",10),("index",0),("0",1),("1",5), ("2",2), ("3",6), ("4",10), ("5",8), ("6",3), ("7",4), ("8",7), ("9",9)]

sigmaBubbleSort2 :: Memoria
sigmaBubbleSort2 = [("size",5),("index",0),("0",1),("1",5), ("2",2), ("3",6), ("4",10)]

bubbleSortPass :: C
bubbleSortPass = Loop (Num 0) (Sub (Var "size") (Num 1)) 
                  (If (Leq (VarDin (Soma (Var "index") (Num 1))) (VarDin (Var "index")))
                     (Seq (Swap (VarDin (Var "index")) (VarDin (Soma (Var "index") (Num 1))))
                        (Atrib (Var "index") (Soma (Var "index") (Num 1))))
                           (Seq Skip (Atrib (Var "index")   
                              (Soma (Var "index") (Num 1)))))

bubbleSort :: (C)
bubbleSort = Loop (Num 0) (Soma (Var "size") (Num 1))
               (Seq bubbleSortPass
                  (Atrib (Var "index") (Num 0)))

programBubbleSort :: (C, Memoria)
programBubbleSort = cbigStep(bubbleSort, sigmaBubbleSort1)

programBubbleSortMem :: Memoria -> (C,Memoria)
programBubbleSortMem ( mem ) = cbigStep(bubbleSort, mem)


sigmaMax :: Memoria
sigmaMax = [("x",20),("y",10),("max",0)]

maxMe :: C
maxMe = If (Leq (Var "x") (Var "y"))
         (Atrib (Var "max") (Var "y"))
            (Atrib (Var "max") (Var "x"))

maxProgram :: (C,Memoria)
maxProgram = cbigStep(maxMe, sigmaMax)