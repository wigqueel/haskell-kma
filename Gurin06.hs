{-# OPTIONS_GHC -Wall #-}
module Petrenko06 where

import Data.Maybe
import qualified Data.Map as M

-- Всі програми і їх елементи являються вірними (well-formed) в наступному значенні:
--   Всі оператори, функції і процедури застосовуються  
--      до вірної кількості аргументів, кожний з яких має відповідний тип.
--   Вирази, які повинні обчислювати логічні значення, будуть завжди обчислюватися
--     до 0 (false) або 1 (true).
--   В присвоєнні масиву  a[i] = e масив завжди визначений (в області дії).
--   Процедура, що викликається як x := p(e1, …, en), завжди буде повертати значення 
--     (закінчує своє обчислення оператором return e) 
--   Оператор return завжди останній оператор для виконання в блоку процедури 
--     (тобто немає "мертвого коду")

--------------------------------------------------------------------
type Id = String
data Value = I Int | A [(Int, Int)]
           deriving (Eq, Show)
data Op = Add | Minus | Mul | Less | Equal | Index
          deriving (Eq, Show)
data Exp = Const Value | 
           Var Id | 
           OpApp Op Exp Exp |
           Cond Exp Exp Exp |
           FunApp Id [Exp] 
         deriving (Eq, Show)

data VarDef = Arr Id | Int Id   deriving (Eq, Show)
type FunDef = (Id, ([VarDef], Exp))

type Binding = M.Map Id Value
type StateP = ([Binding],Binding)
-- st = ([locn,.. loc1], glob)  стек локальних записів активацій + глобальний запис активації

data Statement = Assign Id Exp |
                 AssignA Id Exp Exp |
                 If Exp Block Block |
                 While Exp Block |
                 Call Id Id [Exp] |
                 Return Exp 
               deriving (Eq, Show)

type Block     = [Statement]
type ProcDef   = (Id, ([VarDef], Block))
type Program   = ([VarDef], [FunDef], [ProcDef])

-- Задача 1 -----------------------------------------
getValue ::  Id -> StateP -> Value
getValue name state = M.union (getLocals state) (getGlobals state) M.! name

-- Задача 2 -----------------------------------------
getLocals :: StateP -> Binding
getLocals st =(foldl (M.union)(M.fromList[]) (fst st))

getGlobals :: StateP -> Binding
getGlobals st = snd st

-- Задача 3 -----------------------------------------
assignArray :: Value -> Value -> Value -> Value
assignArray (A arr) (I old) (I new) = A(map (\(x,y) -> if x==old then (x, y*0+new )else (x,y)) arr)
assignArray _ _ _ = I(0)

-- Задача 4 -----------------------------------------
updateVar :: (Id, Value) -> StateP -> StateP
updateVar (i,v) st
 | M.member i gl = (lcl:lcls,(M.insert i v gl))
 |otherwise = ((M.insert i v lcl) : lcls,gl)
  where
   lcls 
    |length (fst st)> 0 = tail $ fst st 
    | otherwise = []
   lcl = getLocals st
   gl = getGlobals st

-- Задача 5 -----------------------------------------
applyOp :: Op -> Value -> Value -> Value
-- Передумова: Значення мають відповідні типи (I або A) для кожної операції
applyOp Add (I a) (I b) = I(a + b)   
applyOp Minus (I a) (I b) = I(a - b)  
applyOp Mul (I a) (I b) = I(a * b)  
applyOp Less (I a) (I b)
 | a<b = I(a)
 | otherwise = I(b)   
applyOp Equal (I a) (I b)
 | a==b = I(1)
 |otherwise = I(0)
applyOp Index (A arr) (I i)
 | length(filter(\(x,_)->x==i) arr) ==1 = I(snd(head(filter(\(x,_)->x==i) arr)))
 |otherwise = I(0)
applyOp _ _ _ = I(0)

-- Задача 6 -----------------------------------------
bindArgs :: [Id] -> [Value] -> Binding
-- Передумова: списки мають однакову довжину
bindArgs lArg lVal = myins [(lArg!!i,lVal!!i)|i<-[0..length lArg-1 ]]


myins :: [(Id,Value)] -> Binding
myins [] = M.empty
myins (x:xs) = M.insert (fst x) (snd x) (myins xs)

-- Задача 7 -----------------------------------------
eval :: Exp -> [FunDef] -> StateP -> Value
eval (Const c) _ _ =  c
eval (Var v) _ st = getValue v st
eval (OpApp op a b) dfx st = applyOp op (eval a dfx st) (eval b dfx st)
eval (Cond e1 e2 e3) dfx st
 | isTrue e1 dfx st = eval e2 [] st
 | otherwise = eval e3 [] st
eval (FunApp f es) dfx st = eval (snd fun) dfx (funArgs:fst st, getGlobals st)
 where
  fun = lookUp f dfx
  vs = evalArgs es dfx st
  funArgs = bindArgs (map myinitv (fst fun)) vs

myinitv::VarDef->Id
myinitv (Arr v) = v
myinitv (Int v) = v

isTrue::Exp->[FunDef]->StateP->Bool
isTrue cond dfx st = not (eval cond dfx st == I(0))

evalArgs :: [Exp] -> [FunDef] -> StateP -> [Value]
evalArgs es dfx st = map evalE es
 where
 evalE e = eval e dfx st 

-- Задача 8 -----------------------------------------
executeStatement :: Statement -> [FunDef] -> [ProcDef] -> StateP -> StateP
executeStatement (Assign idf ex) fs _ stP = updateVar (idf,eval ex fs stP) stP
executeStatement (AssignA idf ex1 ex2) fs _ stP = updateVar (idf,assignArray vv (eval ex1 fs stP) (eval ex2 fs stP)) stP
 where
  vv = getValue idf stP
executeStatement (If ex block1 block2) fs ps stP
 |(eval ex fs stP == I 1) = executeBlock block1 fs ps stP 
 |otherwise = executeBlock block2 fs ps stP
executeStatement statm@(While ex block) fs ps stP
 |(eval ex fs stP == I 1) = executeStatement statm fs ps (executeBlock block fs ps stP)
 |otherwise= stP
executeStatement (Call id1 id2 exps) fs ps stP
 |id1 == "" = rlr $ executeBlock ep fs ps stNew
 |otherwise= rler $ rlrtwo $ executeStatement (Assign id1 (retBlock ep)) fs ps (aempt (executeBlock ep fs ps stNew))
 where
   p = head [m |m<-ps,(fst m == id2)]
   args = fst $ snd p -- ñïèñîê VarDef'îâ
   ep = snd $ snd p --Block
   stNew = updateStack exps args fs (aempt stP)
executeStatement (Return _) _ _ stP = stP 

executeBlock :: Block -> [FunDef] -> [ProcDef] -> StateP -> StateP
executeBlock [] _ _ stP = stP
executeBlock (st:sts) fs ps stP = executeBlock sts fs ps (executeStatement st fs ps stP)

rlr :: StateP -> StateP
rlr st = (tail (fst st),snd st)

rler :: StateP -> StateP
rler st
 |head (fst st)==M.empty = (tail (fst st),snd st) 
 |otherwise= st

rlrtwo :: StateP -> StateP
rlrtwo st = (ll:lll,snd st)
 where
  ll = head (fst st)
  lll
   |length (tail (fst st)) >0 = tail $ tail (fst st)
   |otherwise= []

aempt :: StateP -> StateP
aempt stP = (M.empty:(fst stP),snd stP)

updateStack :: [Exp]->[VarDef]->[FunDef]->StateP->StateP
updateStack [] _ _ stP = stP
updateStack exps vars funs stP = updateStack (tail exps) (tail vars) funs (updateVar (getIdFromVarDef fstVar,eval fstExp funs stP) stP)
 where
  fstExp = head exps
  fstVar = head vars
  
retBlock :: Block -> Exp
retBlock [] = error "Block does not contains return statemant"
retBlock ((Return ex):_) = ex
retBlock ((_):exs) = retBlock exs  

getIdFromVarDef :: VarDef -> Id
getIdFromVarDef (Arr a) = a
getIdFromVarDef (Int a) = a

---------------------------------------------------------------------
-- Допоміжні функції і дані для тестування...
-- Функція пошуку...
lookUp :: (Eq a, Show a) => a -> [(a, b)] -> b
lookUp x t = fromMaybe (error ("\nНе знайдено  " ++ show x )) 
              (lookup x t)

-- Стан для тестування
sampleState :: StateP
sampleState = ([M.fromList [("x",I 5)]], M.fromList [("y",I 2),("a", listToVal [4,2,7])])

sampleState1::StateP
sampleState1 = ([M.fromList [("x",I 5)], M.fromList [("z",I 79)]], M.fromList [("y",I 2),("a", listToVal [4,2,7])])

-- Перетворює список цілих в масив Value...
listToVal :: [Int] -> Value
listToVal xs = A (zip [0..] xs)

 -- Перетворює ціле в Exp...
intToExp :: Int -> Exp
intToExp n = Const (I n)

-- Реалізація виконання програми 
program :: Program -> StateP 
program (dvx, dfx, dpx) = 
   let initv :: VarDef -> (Id, Value)
       initv (Arr v) = (v, A [])
       initv (Int v) = (v, I 0) 
       gl = M.fromList (map initv dvx) 
   in executeStatement (Call "" "main" []) dfx dpx ([],gl)

 -- Assign "len" (Const (I (length arr)+1)),
 --  Assign "i" (Const (I 0)),
 --  While (OpApp Less (Var "i")(Var "len"))
 --  [If (OpApp Index (Var "a")(Var "i"))[Return (Const )]]
 --  )
-- fib чиста функція
-- Функція fib, що обчислює число Фібоначчі
-- func  fib(n) =
--     (n < 3 ? 1 : fib(n-1) + fib(n-2))
fib :: FunDef
fib = ("fib",
     ([Int "n"], Cond (OpApp Less (Var "n") (Const (I 3)))
                  (Const (I 1))
                  (OpApp Add (FunApp "fib" [OpApp Minus (Var "n") (Const (I 1))])
                             (FunApp "fib" [OpApp Minus (Var "n") (Const (I 2))]))
     )
    )

-- Масив
sampleArray :: Exp
sampleArray = Const (listToVal [9,5,7,1])

-- Сума елементів масиву 0..n ...
sumA1 :: ProcDef
sumA1 = ("sumA1",
     ([Arr "a", Int "n"], 
                  [Assign "s" (Const (I 0)),
                   Assign "i" (Const (I 0)),
                   Assign "limit" (OpApp Add (Var "n") (Const (I 1))),
                   While (OpApp Less (Var "i") (Var "limit"))
                         [Assign "s" (OpApp Add (Var "s") 
                                                (OpApp Index (Var "a") (Var "i"))),
                          Assign "i" (OpApp Add (Var "i") (Const (I 1)))
                         ],
                   Return (Var "s")]
     )
    )

-- Додавання двох чисел...
gAdd :: ProcDef
gAdd = ("gAdd", 
     ([Int "x", Int "y"], [Assign "gSum" (OpApp Add (Var "x") (Var "y"))])
    )

-- Повна програма
pr1 :: Program
pr1 = ([Int "gSum"], [], [gAdd, ("main",([],[Call "" "gAdd" [intToExp 5, intToExp 10] ]))])
