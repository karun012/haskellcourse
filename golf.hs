module Golf where

-- | Produces a list of lists with the nth list containing every
-- nth element from the input list
--
-- >>> skips "ABCD" 
-- ["ABCD", "BD", "C", "D"]
--
-- >>> skips "hello!" 
-- ["hello!", "el!", "l!", "l", "o", "!"]
--
-- >>> skips [1] 
-- [[1]]
--
-- >>> skips [True,False] 
-- [[True,False], [False]]
--
-- >>> skips [] 
-- []
--
skips :: [a] -> [[a]]
skips = error "todo"

