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

-- | Converts a list to a Stream
--
-- >>> fromList [1..]
-- 1234
--
fromList :: [a] -> Stream a
fromList (x:xs) = Cons x (fromList xs)

-- | Converts a Stream to a list
--
-- >>> let x = fromList [1..]
-- >>> (take 4 . streamToList) x
-- [1,2,3,4]
--
streamToList :: Stream a -> [a]
streamToList (Cons first rest) = first : (streamToList rest)

instance Show a => Show (Stream a) where
    show (Cons x xs)  = (concat . take 4 . map show . (:) x . streamToList) xs

-- | Generates a stream with infinite copies of the given element
--
-- >>> streamRepeat 4
-- 4444
--
streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

-- | Maps a function over a stream
--
-- >>> (streamMap (+2) . fromList) [1..]
-- 3456
--
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap fn s = (fromList . map fn . streamToList) s

-- | Generates a stream from a seed, and an unfolding rule
--
-- >>> streamFromSeed (+1) 4
-- 4567
--
-- >>> streamFromSeed (id) 9
-- 9999
--
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed fn x = Cons x $ streamFromSeed fn (fn x)
