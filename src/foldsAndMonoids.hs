module FoldsAndMonoids where

import JoinList
import Data.Monoid

-- | Gets the annotation at the root of a JoinList
--
-- >>> tag (Single (Product 5) 'y')
-- Product {getProduct = 5}
--
-- >>> tag (Append (Product 6) (Single (Product 2) 'e') (Single (Product 3) 'a'))
-- Product {getProduct = 6}
--
tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m
