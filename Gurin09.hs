{-# OPTIONS_GHC -Wall #-}
module Gurin09 where
import Data.List hiding (insert, partition)

data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
                deriving (Eq, Ord, Show)

-- ������ 1 -----------------------------------------

isPrefix :: String -> String -> Bool
isPrefix _ [] = False
isPrefix [] _ = True
isPrefix (x:xs) (y:ys) 
    | x == y = isPrefix xs ys
    | otherwise = False

-- ������ 2 -----------------------------------------
partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition [] y = ([], [], y)   
partition x [] = ([], x, []) 
partition x y = f x y 0

f x y i = if (i < length x && i < length y && x!!i == y!!i) then f x y (i + 1) else (take i x, drop i x,drop i y) 


-- ������ 3 -----------------------------------------
suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes (x : xs) = (x : xs) : suffixes xs 

-- ������ 4 -----------------------------------------
isSubstring :: String -> String -> Bool

isSubstring x y 
 | (length y) == (length x) = if y == x then True else False
 | (length y > length x) = if (take (length x) y) == x then True else isSubstring x (tail y)

-- ������ 5 -----------------------------------------
findSubstrings :: String -> String -> [Int]
findSubstrings x y
  = [n | (suf, n) <- (zip (suffixes y) [0..]), isPrefix x suf]

-- ������ 6 -----------------------------------------
getIndices :: SuffixTree -> [Int]
getIndices (Node []) = []
getIndices (Node a) = quicksort(ff a)

ff [] = []
ff ((_, (Leaf x)) : as) = x : ff as
ff ((_, (Node x)) : as) = ff x ++ ff as

-- ������ 7 -----------------------------------------
findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' "" (Leaf a) = [a]
findSubstrings' _  (Leaf _) = []
findSubstrings' ss (Node n) = concat (map getIndex n)
  where
    getIndex (str, t)
      | ff == ""  = getIndices t
      | str' == "" = findSubstrings' ff t
      | otherwise  = []
      where (pre, ff, str') = partition ss str


-- ������ 8 -----------------------------------------
insert :: (String, Int) -> SuffixTree -> SuffixTree
insert (q, w) (Node []) = Node [(q, Leaf w)]
insert (q, w) nd@( Node ((line, t):n) )
  | f1 == ""  = plusn (line, t) (insert (q, w) (Node n))
  | f1 == line = Node ((line, insert (f2, w) t) : n)
  | otherwise  = Node ([(f1, Node [(f2, Leaf w), (f3, t)])]++n)
    where 
      (f1, f2, f3) = partition q line
      plusn nd (Node nds) = Node (nd:nds)

-- �� ������� ������
buildTree :: String -> SuffixTree 
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

-- ������ 9 -----------------------------------------
longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring = undefined
------------------------------------------------------
-- �������� ����� � ��������� �����..

s1 :: String
s1 = "banana"

s2 :: String
s2  = "mississippi"

t1 :: SuffixTree
t1 = Node [("banana", Leaf 0), 
          ("a", Node [("na", Node [("na", Leaf 1), 
                                   ("", Leaf 3)]), 
                     ("", Leaf 5)]), 
          ("na", Node [("na", Leaf 2), 
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2 = Node [("mississippi", Leaf 0), 
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7), 
                      ("", Leaf 10)]), 
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

--exactMatches :: Code -> Code -> Int
--exactMatches (c:cd) (a:att) = if c == a then 1 + exactMatches cd att else exactMatches cd att 
--exactMatches _ _ = 0