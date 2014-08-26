module LazyEvaluation where

import Data.List

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
-- >>> fib 1
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

-- | Generates a stream with an infinite list of natural numbers
--
-- >>> nats
-- 0123
--
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- | Interleaves two streams
--
-- >>> interleave (fromList [2,4..]) (fromList [1,3..])
-- 2143
--
interleave :: Stream a -> Stream a -> Stream a
interleave xs ys = (fromList . concatMap (\(a, b) -> [a, b])) $ zip (streamToList xs) (streamToList ys)

headOr :: Integer -> [Integer] -> Integer
headOr n [] = n
headOr _ xs = head xs

-- | Computes the largest power of 2 that divides the given number evenly
--
-- >>> largestPowerOfTwoThatDividesEvenly 4
-- 2
--
-- >>> largestPowerOfTwoThatDividesEvenly 2
-- 1
--
-- >>> largestPowerOfTwoThatDividesEvenly 6
-- 1
--
-- >>> largestPowerOfTwoThatDividesEvenly 0
-- 0
--
-- >>> largestPowerOfTwoThatDividesEvenly 8
-- 3
--
largestPowerOfTwoThatDividesEvenly :: Integer -> Integer
largestPowerOfTwoThatDividesEvenly 0 = 0
largestPowerOfTwoThatDividesEvenly n = ((+1) . (headOr 0) . map toInteger . findIndices (==0) . map (flip mod n) . map (2^)) [1..n]

-- | Ruler function
--
-- (take 16 . streamToList) ruler
-- [0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4]
--
ruler :: Stream Integer
ruler = interleave (streamRepeat 0) ((fromList . map largestPowerOfTwoThatDividesEvenly) [2,4..])
