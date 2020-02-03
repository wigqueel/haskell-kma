{-# OPTIONS_GHC -Wall #-}
module HWI02 where

-- ������ 1 -----------------------------------------
sumFr :: [Integer] -> Integer
f1 :: Integer -> Integer -> Integer
sumFr = foldr f1 0
f1 a s = s + a
  
-- ������ 2 ----------------------------------------- 
factorial :: Integer -> Integer
f2 :: Integer-> Integer-> Integer
factorial n = foldl f2 1 [1..n]
f2 a s = a * s

-- ������ 3 -----------------------------------------
concatFr :: [Integer] -> [Integer] -> [Integer]
f3:: Integer -> [Integer] -> [Integer]
concatFr ys xs = foldr f3 xs ys
f3 a xs = a:xs

-- ������ 4 -----------------------------------------
sortInsert :: [Integer] -> [Integer]
insert :: [Integer] -> Integer -> [Integer]
sortInsert  = foldl insert [] 
insert [] v = [v]
insert (x:xs) v = if v < x then v:x:xs else x : insert xs v

-- ������ 5 -----------------------------------------
map2 :: (a->b->c) -> [a] -> [b] -> [c]
map2 _ _ [] = []
map2 _ [] _ = []
map2 f (x:xs) (y:ys) = f x y : map2 f xs ys

-- ������ 6 -----------------------------------------
expPart :: Integer -> Integer -> Double
expPart a b = sum [ (fromInteger a^i) / fromInteger (factorial i) | i <- [1..b]]

-- ������ 7 -----------------------------------------
triangle :: [Integer]
triangle = scanl (+) 1 [2..]

-- ������ 8 -----------------------------------------
piramid :: [Integer]
piramid = scanl (+) 1 [a*a | a <- [1..]]

-- ������ 9 -----------------------------------------
indexes :: [Int] -> [Int] -> [Int]
indexes _ [] = []
indexes [] ys = [0..(length ys)]
indexes xs ys= [ i| i <- [0.. length ys], xs == take (length xs) (drop i ys) ]