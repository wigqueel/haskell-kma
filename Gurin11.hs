{-# OPTIONS_GHC -Wall #-}
module HGurin11 where

import Data.Char(isLower)
import Data.List
import Data.Maybe

data BExp = Bvalue Bool | Bvar Char | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)
type Env = [(Char, Bool)]

type NodeId = Int
type BDDNode =  (NodeId, (Char, NodeId, NodeId))
type BDD = (NodeId, [BDDNode])

-- ������ 1 -----------------------------------------
checkSat :: BDD -> Env -> Bool
checkSat (root, nodes) env = f root 
   where 
      f 0 = False
      f 1 = True
      f id
         | bac a env  = f r
         | otherwise     = f l
           where
            (a, l, r) = bac id nodes

-- ������ 2 -----------------------------------------
sat :: BDD -> [[(Char, Bool)]]
sat bdd@(node, bnodes) = filter (checkSat bdd) allPoss 
  where allPoss = [zipWith (\a b -> (a, b)) allNodes bs| bs <- allBs]
        allBs = pos (length allNodes)
        allNodes = alln bnodes []
           where alln [] s = sort s
                 alln ((root, (a, l, r)):xs) s
                   | a `elem` s = alln xs s
                   | otherwise = alln xs (a : s)

pos n = init (map (map (\x -> if x == 1 then True else False)) [b a n | a <- [0..2^n]])
b x n = f1 x n []
          where f1 _ 0 s = s
                f1 x n s = f1 (x `div` 2) (n-1) ((x `mod` 2) : s)

-- ������ 3 -----------------------------------------
simplify :: BExp -> BExp
simplify (Not (Not (Bvalue b))) = Bvalue b
simplify (Not (Bvalue True)) = Bvalue False
simplify (Not (Bvalue False)) = Bvalue True
simplify (And (Bvalue True) (Bvalue b)) = Bvalue b
simplify (And (Bvalue False) (Bvalue _)) = Bvalue False
simplify (Or (Bvalue _) (Bvalue True)) = Bvalue True
simplify (Or (Bvalue b) (Bvalue False)) = Bvalue b
simplify q = q

-- ������ 4 -----------------------------------------
restrict :: BExp -> Char -> Bool -> BExp
restrict xs@(Bvalue _) _ _  = xs
restrict xs@(Bvar a) i b = if a == i then Bvalue b else xs
restrict (Not x) a b   = simplify  (Not (restrict x a b))
restrict (Or  x y) a b = simplify (Or  (restrict x a b) (restrict y a b))
restrict (And x y) a b = simplify (And (restrict x a b) (restrict y a b))

-- ������ 5 -----------------------------------------
-- ����������: ����� ����� (�����) � �������� ����� (BExp) �"��������� 
--    ����� ���� ��� � ������ ������ (Char); ���� ����� �������� --if e == Bvalue True then (1, []) else (0,[])
buildBDD :: BExp -> [Char] -> BDD
buildBDD a xs = buildBDD' a 2 xs

buildBDD' :: BExp -> NodeId -> [Char] -> BDD
buildBDD' a _ []= if a == Bvalue True then (1, []) else (0,[])
buildBDD' a n (x : xs) = (n, (n, (x, l, r)) : ln ++ rn)
  where 
    (l, ln)   = buildBDD' (restrict a x False) (2 * n) xs
    (r, rn) = buildBDD' (restrict a x True) (2 * n + 1) xs

-- ������ 6 -----------------------------------------
-- ����������: ����� ����� (�����) � �������� ����� (BExp) �"��������� 
--    ����� ���� ��� � ������ ������ (Char); ���� ����� ��������
buildROBDD :: BExp -> [Char] -> BDD
buildROBDD = undefined

-- ������ 7 -----------------------------------------
fullBexp :: String -> Maybe BExp 
fullBexp = undefined 

bexp :: String -> Maybe (BExp,String)
bexp = undefined

bcon :: String -> Maybe (BExp,String)
bcon = undefined

manyCon :: (BExp,String) -> Maybe (BExp,String)
manyCon = undefined

bdis :: String -> Maybe (BExp,String)
bdis = undefined

manyDis :: (BExp,String) -> Maybe (BExp,String)
manyDis = undefined

------------------------------------------------------
-- �������� ��� ����������..
bs1, bs2, bs3, bs4, bs5, bs6, bs7, bs8, bs9 :: String
bs1 = "F"
bs2 = "!(x&(F|y))"
bs3 = "u&T"
bs4 = "d&(x|!y)"
bs5 = "!(d&(x|!y))"
bs6 = "u&x|y&z" 
bs7 = "!y|(x|!e)"
bs8 = "u|!u"
bs9 = "z&(y|!y&x)"

b1, b2, b3, b4, b5, b6, b7, b8, b9 :: BExp
b1 = Bvalue False
b2 = Not (And (Bvar 'x') (Or (Bvalue False) (Bvar 'y')))
b3 = And (Bvar 'u') (Bvalue True)
b4 = And (Bvar 'd') (Or (Bvar 'x') (Not (Bvar 'y')))
b5 = Not (And (Bvar 'd') (Or (Bvar 'x') (Not (Bvar 'y'))))
b6 = Or (And (Bvar 'u') (Bvar 'x')) (And (Bvar 'y') (Bvar 'z'))
b7 = Or (Not (Bvar 'y')) (Or (Bvar 'x') (Not (Bvar 'e')))
b8 = Or (Bvar 'u') (Not (Bvar 'u'))
b9 = And (Bvar 'z') (Or (Bvar 'y') (And (Not (Bvar 'y')) (Bvar 'x')))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8, bdd9 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(2,('x',4,5)),(4,('y',1,1)),(5,('y',1,0))])
bdd3 = (5,[(5,('u',0,1))])
bdd4 = (2,[(2,('x',4,5)),(4,('y',8,9)),(8,('d',0,1)),(9,('d',0,0)),
           (5,('y',10,11)),(10,('d',0,1)),(11,('d',0,1))])
bdd5 = (3,[(4,('y',8,9)),(3,('x',4,5)),(8,('d',1,0)),(9,('d',1,1)),
           (5,('y',10,11)),(10,('d',1,0)),(11,('d',1,0))])
bdd6 = (2,[(2,('u',4,5)),(4,('x',8,9)),(8,('y',16,17)),(16,('z',0,0)),
           (17,('z',0,1)),(9,('y',18,19)),(18,('z',0,0)),(19,('z',0,1)),
           (5,('x',10,11)),(10,('y',20,21)),(20,('z',0,0)),(21,('z',0,1)),
           (11,('y',22,23)),(22,('z',1,1)),(23,('z',1,1))])
bdd7 = (6,[(6,('x',4,5)),(4,('y',8,9)),(8,('e',1,1)),(9,('e',1,0)),
           (5,('y',10,11)),(10,('e',1,1)),(11,('e',1,1))])
bdd8 = (2,[(2,('u',1,1))])
bdd9 = (2,[(2,('x',4,5)),(4,('y',8,9)),(8,('z',0,0)),(9,('z',0,1)),(5,('y',10,11)),(10,('z',0,1)),(11,('z',0,1))])


bac :: Eq a => a -> [(a, b)] -> b
bac x ys
  = fromJust(lookup x ys) 

