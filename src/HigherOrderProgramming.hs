module HigherOrderProgramming where

-- | Helper functions
-- 
-- >>> cartProd [1,2] ['a','b']
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
--
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

-- | Runs (x - 2) on all even numbers, and sums them up
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
fun1' = product . map (2 -) . filter even

-- | Some crazy integer manipulation
--
-- >>> fun2 9
-- 276
--
-- >>> fun2 12
-- 58
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

-- | A more wholemeal implementation of fun2
--
-- >>> fun2' 9
-- 276
--
-- >>> fun2' 12
-- 58
fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (> 1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

-- | Implementation of xor using fold
--
-- >>> xor [False, True, False]
-- True
-- 
-- >>> xor [False, True, False, False, True]
-- False
--
-- >>> xor [True, True, False, False, True, True, False, False, True]
-- True
--
xor :: [Bool] -> Bool
xor = not . foldl(==) False

-- | Implementing map using fold
--
-- >>> map' (+2) [1,2,3,4]
-- [3,4,5,6]
--
-- >>> map' (++ "!") ["BIFF", "BANG", "POW"]  
-- ["BIFF!","BANG!","POW!"] 
--
-- >>> map' (replicate 3) [3..6]  
-- [[3,3,3],[4,4,4],[5,5,5],[6,6,6]]
--
map' :: (a -> b) -> [a] -> [b]
map' fn = foldr((:) . fn) []

-- | Implementation of foldl using foldr
--
-- >>> foldl' (\_ x -> x) 0 [1,2,3,4]
-- 4
--
-- >>> foldl (\acc x -> x : acc) [] [1,2,3,4]
-- [4,3,2,1]
--
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' fn base xs = foldr(flip fn) base $ reverse xs

-- | Generates all odd primes up to 2n + 2
-- 
-- >>> sieveSundaram 2
-- [1,3,5]
--
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = filter odd . map snd . filter ((==) 0 . uncurry (flip mod)) $ cartProd [1,n] [1..(2 * n + 2)]
