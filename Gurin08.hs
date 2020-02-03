{-# OPTIONS_GHC -Wall #-}
module Gurin08 where

import Data.Tree

ot1, ot2, ot3 :: Tree Char

ot1 = Node 'A' []
ot2 = Node 'A' [Node 'B'[]]
ot3 = Node 'A' [Node 'B'[ Node 'C'[]]]
ot4 = Node 'A' [Node 'B'[ Node 'D'[]]]

ft1 :: Forest Char
ft1 = [ot2, ot3, ot1]

data BTree a = BEmpty | BNode a (BTree a) (BTree a)
               deriving (Show, Eq)

-- ������ 1 -----------------------------------------
dfsForest ::  Forest a -> [a]  
dfsForest [] = []
dfsForest (Node x xs:[]) = x : dfsForest xs
dfsForest (Node x xs:ts) = x : (dfsForest xs) ++ dfsForest ts

-- ������ 2 ----------------------------------------- 
bfsForest ::  Forest a -> [a]
bfsForest [] = []
bfsForest (Node x xs:ts) = x : (bfsForest (ts ++ xs))

-- ������ 3 -----------------------------------------
equelTree (Node x xs) (Node y ys) = x == y && lTree xs ys

lTree [] [] = True
lTree (x : xs) (y : ys) = equelTree x y && lTree xs ys
lTree _ _ = False

isInTree :: (Eq a) => Tree a -> Tree a -> Bool
isInTree x y =
	if equelTree x y == True then True
	else if ((True `elem` (map (isInTree x) (listTrees y))) == True) then True
	else False

listTrees (Node x []) = []
listTrees (Node _ y) = y


-- ������ 4 -----------------------------------------
toBTree :: Forest a -> BTree a
toBTree [] = BEmpty
toBTree (x:xs) = BNode (get x) (toBTree (leaves x)) (toBTree xs)

get :: Tree a -> a
get (Node x _) = x

-- ������ 5 -----------------------------------------
fromBTree :: BTree a -> Forest a  
fromBTree BEmpty = []
fromBTree (BNode x lt rt) = Node x (fromBTree lt) : (fromBTree rt)

-- ������ 6 -----------------------------------------

isSearch :: (Ord a) => BTree a -> Bool
isSearch BEmpty = True
isSearch (BNode _ BEmpty BEmpty) = True
isSearch (BNode x tl tr) = (f (>x) tr)&&(f (<x) tl)&&(isSearch tl)&&(isSearch tr)

f :: (a -> Bool) -> BTree a -> Bool
f _ BEmpty = True
f a (BNode x lt rt) = if a x then (f a lt)&&(f a rt) else False

-- ������ 7  -----------------------------------------
elemSearch ::(Ord a) => BTree a -> a -> Bool
elemSearch BEmpty _ = False
elemSearch (BNode x tl tr) elem  = if (x/=elem) then if (x > elem) then elemSearch tl elem else elemSearch tr elem else True

-- ������ 8 ------------------------------------------
insSearch :: (Ord a) => BTree a -> a -> BTree a
insSearch BEmpty x = BNode x BEmpty BEmpty
insSearch (BNode x lt rt) a = if a < x then BNode x (insSearch lt x) rt else if a > x then BNode x lt (insSearch rt x) else BNode a lt rt
-- ������ 9 ------------------------------------------
delSearch :: (Ord a) => BTree a -> a -> BTree a
delSearch = undefined

-- ������ 10 -----------------------------------------
ff :: (Ord a) => BTree a -> [a]
ff BEmpty = []
ff (BNode x lt rt) = (ff lt) ++ [x] ++ (ff rt)

sortList :: (Ord a) => [a] -> [a]
sortList list = ff (foldl insSearch BEmpty list)
--
leaves :: Tree a -> [Tree a]
leaves (Node _ a) = a

