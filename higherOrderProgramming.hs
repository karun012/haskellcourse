module HigherOrderProgramming where

-- | Runs (x - 2) on all even numbers
--
-- >>> fun1 [10,12,14,16]
-- 13440
--
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

-- | A more wholemeal implementation of fun1
--
-- >>> fun1' [10,12,14,16]
-- 13440
--
fun1' :: [Integer] -> Integer
fun1' = product . map ((-) 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)
    

