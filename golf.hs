module Golf where

import Data.List

-- | Helper functions
--
-- >>> ifFirstIsEqualToOne (1, 1)
-- True
--
-- >>> ifFirstIsEqualToOne (0, 1)
-- False
--
ifFirstIsEqualToOne :: (Int, b) -> Bool
ifFirstIsEqualToOne = (==) 1 . fst

-- | Produce a list of n zeros and a one at the end
--
-- >>> nZerosAndAOne 2
-- [0,0,1]
--
-- >>> nZerosAndAOne 4
-- [0,0,0,0,1]
--
nZerosAndAOne :: Int -> [Int]
nZerosAndAOne n = replicate n 0 ++ [1]

-- >>> everyNth 2 [1,2,3,4]
-- [2,4]
--
everyNth :: Int -> [a] -> [a] 
everyNth n = map snd . filter ifFirstIsEqualToOne . zip (cycle (nZerosAndAOne (n - 1)))

-- | Produces a list of lists with the nth list containing every
-- nth element from the input list
--
-- >>> skips [] 
-- []
--
-- >>> skips [1] 
-- [[1]]
--
-- >>> skips [True,False] 
-- [[True,False],[False]]
--
-- >>> skips "ABCD" 
-- ["ABCD","BD","C","D"]
--
-- >>> skips "hello!" 
-- ["hello!","el!","l!","l","o","!"]
--
skips :: [a] -> [[a]]
skips [] = []
skips xs = zipWith everyNth [1..length xs] (replicate (length xs) xs)


-- | Finds the local maxima of a list
-- 
-- >>> localMaxima [2,9,5,6,1] 
-- [9,6]
--
-- >>> localMaxima [2,3,4,1,5]
-- [4]
--
-- >>> localMaxima [1,2,3,4,5]
-- []
localMaxima :: [Integer] -> [Integer]
localMaxima [x,y,z] = if (x < y && y > z) then [y] else []
localMaxima xs = case length xs > 3 of
                 True -> localMaxima (take 3 xs) ++ localMaxima (drop 1 xs)
                 False -> []

rows :: [[Integer]] -> [[Integer]]
rows xs = case length xs > 0 of 
          True -> concatMap (take 1) xs : rows (filter (not . null) (map (drop 1) xs))
          _ -> []

drawRow :: [Integer] -> String
drawRow xs = concatMap (\x -> if x `elem` xs then "*" else " ") [0..9] ++ "\n"

drawRows :: [[Integer]] -> String
drawRows = concatMap drawRow

-- | Draws a vertical histogram
--
-- >>> histogram [3,5]
-- "   * *    \n==========\n0123456789\n"
-- 
histogram :: [Integer] -> String
histogram = (++ "==========\n0123456789\n") . drawRows . rows . group . sort
