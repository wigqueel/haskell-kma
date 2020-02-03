{-# OPTIONS_GHC -Wall #-}
module Petrenko05 where

data AbstractInteger = Zero
                     | Succ AbstractInteger
                     | Pred AbstractInteger
                     deriving (Show, Eq)

-- Задача 1 -----------------------------------------
instance Ord AbstractInteger where
   (<=) x y = compare x y /=GT
   (>=) x y = compare x y /=LT
   (<) x y = compare x y ==LT
   (>) x y = compare x y ==GT

-- Задача 2 ----------------------------------------
aiToInteger :: AbstractInteger -> Integer
aiToInteger Zero = 0
aiToInteger (Succ(x)) = (aiToInteger (x)) +1
aiToInteger (Pred(x)) = (aiToInteger (x)) -1
 
-- Задача 3 -----------------------------------------
plusAbs :: AbstractInteger -> AbstractInteger -> AbstractInteger
plusAbs Zero Zero = Zero
plusAbs (a) Zero  = a
plusAbs Zero (a) = a
plusAbs (Pred a) (Succ b) = plusAbs a b
plusAbs (Succ a) (Pred b) = plusAbs a b
plusAbs (Succ a) (Succ b) = Succ(Succ(plusAbs a b))
plusAbs (Pred a) (Pred b) = Pred(Pred(plusAbs a b))

-- Задача 4 -----------------------------------------
timesAbs :: AbstractInteger -> AbstractInteger -> AbstractInteger
timesAbs Zero _ = Zero
timesAbs x (Succ y) = plusAbs x (timesAbs x y)
timesAbs x@(Pred _) y@(Pred _) = timesAbs (myflip x) (myflip y)
timesAbs x y = timesAbs y x

myflip :: AbstractInteger -> AbstractInteger
myflip (Pred ai) = Succ (myflip ai)
myflip ai = ai

myNegate :: AbstractInteger -> AbstractInteger
myNegate Zero = Zero
myNegate (Succ a) = Pred(myNegate a)
myNegate (Pred a) = Succ(myNegate a)

myFromInteger :: Integer -> AbstractInteger
myFromInteger 0 = Zero
myFromInteger a
 |a < 0 = Pred(myFromInteger $a+1)
 |otherwise = Succ(myFromInteger $a-1)

myAbs :: AbstractInteger -> AbstractInteger
myAbs Zero = Zero
myAbs (Succ a) = (Succ a)
myAbs (Pred a) = myNegate(Pred a)

mySignum :: AbstractInteger -> AbstractInteger
mySignum Zero = Zero
mySignum a 
  | (aiToInteger a) > 0 = Succ Zero
  | (aiToInteger a) == 0 = Zero
  | (aiToInteger a) < 0 = Pred Zero
mySignum _ = Zero

-- Задача 5 -----------------------------------------
instance Num AbstractInteger  where
    (+)   = plusAbs
    (*)   = timesAbs
    negate      = myNegate 
    fromInteger = myFromInteger
    abs         = myAbs
    signum      = mySignum

-- Задача 6 -----------------------------------------
factorial :: (Eq a, Num a) => a -> a
factorial 0 = 1
factorial a = a * factorial(a-1)

-- Задача  7 -----------------------------------------
data Quaternion = Quaternion Double Double Double Double deriving (Eq)

instance Show Quaternion where
    -- show (Quaternion a b c d) = show a ++"+"
    show (Quaternion a b c d) = show a++"+"++show b++"i+"++show c++"j+"++show d++"k"

-- Задача 8 -----------------------------------------
plusQuaternion :: Quaternion -> Quaternion -> Quaternion
plusQuaternion (Quaternion a b c d) (Quaternion e f g h) = (Quaternion (a+e) (b+f) (c+g) (d+h))

-- Задача 9 -----------------------------------------
timesQuaternion :: Quaternion -> Quaternion -> Quaternion
timesQuaternion (Quaternion r i j k) (Quaternion a b c d) = Quaternion cntr cnti cntj cntk
              where 
                cntr = r * a - i * b - j * c - k * d
                cnti = r * b + i * a + j * d - k * c
                cntj = r * c + k * a - i * d + k * b
                cntk = r * d + k * a + i * c - j * b

--- Задача 10 ----------------------------------------
divQ :: Quaternion -> Double -> Quaternion
divQ (Quaternion r i j k) x = Quaternion (r / x) (i / x) (j / x) (k / x)

instance Num Quaternion  where
    (+)                         = plusQuaternion
    (*)                         = timesQuaternion
    negate (Quaternion r i j k) = Quaternion r (negate i) (negate j) (negate k)
    fromInteger r               = Quaternion (fromInteger r) 0 0 0
    abs    (Quaternion r i j k) = Quaternion (sqrt $ r*r + i*i + j*j + k*k) 0 0 0
    signum q                    = divQ q ((\(Quaternion r _ _ _) -> r) (abs q))
