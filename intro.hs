module Intro where

-- Helper functions

charToInt :: Char -> Integer
charToInt n = (read [n]) :: Integer

modString :: Char -> Integer -> Integer
modString = mod . charToInt

reverse' :: [Integer] -> [Integer]
reverse' xs = error "todo"

-- | Convert a number to a list of digits
--
-- Examples:
--
-- >>> toDigits 1234
-- [1,2,3,4]
--
-- >>> toDigits 0
-- []
--
-- >>> toDigits (-17)
-- []
--
toDigits :: Integer -> [Integer]
toDigits n = case n > 0 of 
             True -> foldr ((:) . (flip modString 10)) [] (show n)
             _ -> []

-- | Convert a number to a list of digits, and reverse the list
--
-- Examples:
--
-- >>> toDigitsRev 1234
-- [4,3,2,1]
-- 
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- | Double every other number in a list
--
-- Examples:
--
-- >>> doubleEveryOther [1,2,3]
-- [1,4,3]
--
-- >>> doubleEveryOther [4,5,6,7]
-- [4,10,6,14]
--
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith (*) (cycle [1,2])


-- | Sum all the digits in a list
--
-- Examples:
--
-- >>> sumDigits [1,2,3]
-- 6
--
-- >>> sumDigits [1, 12, 123]
-- 10
--
sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . map toDigits

-- | Validates a credit card number based on the conditions set in the assignment
--
-- Examples:
--
-- >>> validate 4012888888881881
-- False
--
-- >>> validate 4012888888881882
-- True
--
validate :: Integer -> Bool
validate = (==) 0 . flip rem 10 . sumDigits . toDigitsRev
