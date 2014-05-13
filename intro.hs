charToInt :: Char -> Integer
charToInt n = (read [n]) :: Integer

modString :: Char -> Integer -> Integer
modString = mod . charToInt

toDigits :: Integer -> [Integer]
toDigits n = case n > 0 of 
             True -> foldr ((:) . (flip modString 10)) [] (show n)
             _ -> []

reverse' :: [Integer] -> [Integer]
reverse' xs = error "todo"

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits
