{-# OPTIONS_GHC -Wall #-}
module Gurin07 where

data Stream a = Cons a (Stream a)

-- ��������� Show �������� ����� 20 ��������, �� ����� ����������� ������ �����������
instance Show a => Show (Stream a) where
    show xs =  (foldl (++) "[" 
                  $ map (\x -> (show x) ++ ", ") $ take 20 $ streamToList xs
                ) ++ "..."

-- 1 -----------------------------------------

streamToList :: Stream a -> [a]
streamToList (Cons a b) = a : streamToList b

-- ������ 2 -----------------------------------------
instance Functor Stream where
    fmap f (Cons x s) = Cons (f x) (fmap f s)

-- ������ 3 -----------------------------------------
sRepeat :: a -> Stream a
sRepeat x = Cons x $ sRepeat x

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x (sIterate f $ f x)

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x xs) y = Cons x (sInterleave y xs)

sTake :: Int -> Stream a -> [a]
sTake n _ | n <=0 = []
sTake n (Cons x xs) = x : sTake (n-1) xs

-- ������ 4 -----------------------------------------
nats :: Stream Integer
nats =  sIterate (1 +) 0

-- ������ 5 -----------------------------------------
ruler :: Stream Integer
ruler = rulerr 0
    where
        rulerr n = sInterleave (sRepeat n) $rulerr $n + 1
-- ������ 6 -----------------------------------------
rand :: Integer -> Stream Integer
rand r = sIterate randr $ randr r
  where 
        randr n = ((1103515245 * n) + 12345) `mod`2147483648

-- ������ 7 -----------------------------------------
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1 
fib 2 = 1
fib n = fib( n -1 )+ fib (n -2  )

fibs1 :: [Integer]
fibs1 = map fib [1..]

-- ������ 8 -----------------------------------------
fibs2 :: [Integer]
fibs2 = 1 : 1 : (zipWith (+) fibs2 $ tail $fibs2)

-- ������ 9 -----------------------------------------
data  Matrix a = M(a,a)(a,a)
         deriving (Show, Eq, Ord)
         
instance Num a => Num (Matrix a) where
    (+) (M(a,b)(c,d)) (M(a1,b1)(c1,d1)) = M(a+a1,b+b1)(c+c1,d+d1)
    (*) (M(a,b)(c,d)) (M(a1,b1)(c1,d1)) = M(a*a1 + b*c1,a*b1 + b*d1)(c*a1 + d*c1,c*b1 + d*d1)
    negate (M(a,b)(c,d)) = (M(-a,-b)(-c,-d))
    fromInteger n =  M(fromInteger n,fromInteger 0)(fromInteger 0,fromInteger n)
    -- ������������ �� �������
    abs    = undefined
    signum = undefined

-- ������ 10 ----------------------------------------
fastFib :: Integer -> Integer
fastFib n = func $ (M (1,1) (1,0) ) ^n
        where
            func (M _ x) = fst x