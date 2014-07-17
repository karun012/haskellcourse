module LazyEvaluation where

-- | Computes the nth fibonacci number recursively
--
-- >>> fib 0
-- 0
--
-- >>> fib 1
-- 1
--
-- >>> fib 2
-- 1
--
-- >>> fib 14
-- 377
--
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

-- | Generates an infinite list of fibonacci numbers
--
-- >>> take 1 $ fibs1
-- [0]
--
-- >>> take 5 $ fibs1
-- [0,1,1,2,3]
--
fibs1 :: [Integer]
fibs1 = map fib [0..]


-- | Faster implementation of fibs1
--
-- >>> take 1 $ fibs2
-- [0]
--
-- >>> take 5 $ fibs2
-- [0,1,1,2,3]
--
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

data Stream a = Cons a (Stream a)

fromList :: [a] -> Stream a
fromList (x:xs) = Cons x (fromList xs)

streamToList :: Stream a -> [a]
streamToList (Cons first rest) = first : (streamToList rest)

instance Show a => Show (Stream a) where
    show (Cons x xs)  = (concat . take 4 . map show . (:) x . streamToList) xs
