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
