{-# OPTIONS_GHC -Wall #-}
module HWI04 where

type Graph  = [[Int]]

gr1, gr2, gr3, gr4:: Graph
gr1 = [[1,2,3],[2,3],[3,4],[4],[]]
gr2 = [[3,4],[0,3],[0,1,4],[2,4],[1]]
gr3 = [[1],[2],[3],[1],[0,3]]
gr4 = [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[0,1,2,3]]

isGraph :: Graph -> Bool
isGraph gr = if elem [] gr then True else False

isTournament :: Graph -> Bool 
edges :: Graph -> [(Int,Int)]
edges g = [(x,y)| x <- [0..(length g -1)], y <- g!!x] 
a:: Graph -> Int
a [] = 0
a (g:gr) = length (g:gr) + length gr +1

isTournament gr = if length (edges gr) == a gr then True else False

-- ������ 3 ------------------------------------
isTransitive :: Graph -> Bool
isTransitive [] = False
isTransitive [[]] = False
isTransitive gr =
  foldl
    (&&)
    True
    [ (fst u_v, snd v_w) `elem` (allEdges gr)
    | u_v <- (allEdges gr)
    , v_w <- (allEdges gr)
    , (snd u_v == fst v_w)
    ]
allEdges :: Graph -> [(Int, Int)]
allEdges gr = [(x, y) | x <- (allNodes gr), y <- gr !! x]

-- ������ 4 ------------------------------------
buildTransitive :: Graph -> Graph 
allNodes :: Graph -> [Int]
allNodes graph = [0 .. ((length graph) - 1)]
buildRes :: [[[Int]]] -> [Int]
buildRes ways = sortInsert (set (foldl1 (++) (diffWays ways)))

sortInsert :: [Int] -> [Int]
sortInsert [] = []
sortInsert xs = foldl insert [] xs
insert :: [Int] -> Int -> [Int]
insert xs x = (filter (<= x) xs) ++ [x] ++ (filter (> x) xs)

diffWays :: [[[Int]]] -> [[Int]]
diffWays ways = [x | y <- ways, g <- y, g /= [], let x = (init g)]
buildTransitive gr = map buildRes (map (allWays gr) (allNodes gr))

-- ������ 5 ------------------------------------
allWays:: Graph ->Int ->[[[Int]]]
allWays gr v = until condW(stepW gr)[[[v]]]
condW::([[[Int]]]) -> Bool
condW wss = null (head wss)
stepW ::Graph -> [[[Int]]] -> [[[Int]]]
stepW gr wss@(wsn:_) = [t:w | w@(v:vs) <- wsn, notElem v vs, t <- gr!!v]:wss
stepW gr [] = error "allWays:stepW"

qq :: Graph -> Int -> Int -> [[Int]]
qq gr from to =
  [way | ways <- allWays gr from, way <- ways, (head way) == to]

longWay :: Graph -> Int -> Int -> Maybe [Int] 
longWay gr from to
  | null (qq gr from to) = Nothing
  | null (head (qq gr from to)) = Nothing
  | otherwise = Just $ reverse $ head $ (qq gr from to)
-- ������ 6 ------------------------------------
gamiltonWay :: Graph -> Maybe [Int]
gamiltonWay = undefined

-- ������ 7 ------------------------------------
isAcyclic :: Graph -> Bool 
isAcyclic = undefined

-- ������ 8 ------------------------------------
topolSort :: Graph -> Maybe [Int] 
topolSort = undefined

-- ������ 9------------------------------------
isTopolSort :: Graph -> [Int] -> Bool 
isTopolSort = undefined

set :: [Int] -> [Int]
set [] = []
set (x:xs) = x : (set (remove x xs))
remove :: Int -> [Int] -> [Int]
remove _ [] = []
remove x (y:ys)
  | x == y = remove x ys
  | otherwise = y : (remove x ys)
