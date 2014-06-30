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
