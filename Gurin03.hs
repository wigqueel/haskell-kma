{-# OPTIONS_GHC -Wall #-}
module HW03 where

-- ��� - ������ ������ ������� - ���������� ���� '0' ..'9'
type Code = String

-- ���� ��� (Move) ���� ����������� Move �������������� ������ (Code) � ��� �����:  
--    ������� "����" � "����"  � ����������-����� �� ��������� �� ����-����� 
data Move = Move Code Int Int
          deriving (Show, Eq)

-- ������ 1 -----------------------------------------
exactMatches :: Code -> Code -> Int
exactMatches (c:cd) (a:att) = if c == a then 1 + exactMatches cd att else exactMatches cd att 
exactMatches _ _ = 0

-- ������ 2 -----------------------------------------
countDigits :: Code -> [Int]
f2 :: Code -> Char -> Int
f2 (x:xs) y = if x == y then 1+f2 xs y else f2 xs y
f2 [] _ = 0
countDigits cd = map (f2 cd) "0123456789"

-- ������ 3 ----------------------------------------- 

matches :: Code -> Code -> Int
matches xs ys = sum $ f3 (countDigits xs) (countDigits ys)
f3 :: [Int] -> [Int] -> [Int]
f3 (x:xs) (y:ys) = (min x y) : f3 xs ys
f3 _ _ = []

 
-- ������ 4 -----------------------------------------
getMove :: Code -> Code -> Move
getMove cd att = Move att (exactMatches cd att) (matches cd att - exactMatches cd att)

-- ������ 5 -----------------------------------------
isConsistent :: Move -> Code -> Bool
f4,f5 :: Move -> Int
f4 (Move _ f _) = f 
f5 (Move _ _ p) = p
isConsistent (Move att f p) cd = if (f4 (getMove cd att) == f && f5(getMove cd att) == p) then True else False

-- ������ 6 -----------------------------------------
filterCodes :: Move -> [Code] -> [Code]
filterCodes mv cdx = filter (isConsistent mv) cdx 

-- ������ 7 -----------------------------------------
allCodes :: Int -> [Code]
a :: Int -> [Int]
a b = [0..]
allCodes n = filter (length (a 3)< n) (a 3)
   
-- ������ 8 -----------------------------------------
solve :: Code -> [Move]
solve = undefined
 