{-# LANGUAGE DataKinds #-}
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

-- | Joins two JoinLists
--
-- >>> let joinListSample = (Single (Product 5) 'y') +++ (Single (Product 3) 'z')
-- >>> tag joinListSample
-- Product {getProduct = 15}
--
-- >>> let joinListSample = ((Append (Product 6) (Single (Product 2) 'e') (Single (Product 3) 'a'))) +++ (Single (Product 3) 'z')
-- >>> tag joinListSample
-- Product {getProduct = 18}
--
-- >>> let joinListSample = (Single (Product 3) 'z') +++ ((Append (Product 6) (Single (Product 2) 'e') (Single (Product 3) 'a'))) 
-- >>> tag joinListSample
-- Product {getProduct = 18}
--
-- >>> let joinListSample = ((Append (Product 6) (Single (Product 2) 'e') (Single (Product 3) 'a'))) +++ ((Append (Product 6) (Single (Product 2) 'e') (Single (Product 3) 'a'))) 
-- >>> tag joinListSample
-- Product {getProduct = 36}
--
-- >>> let joinListSample = (Empty) +++ ((Append (Product 6) (Single (Product 2) 'e') (Single (Product 3) 'a'))) 
-- >>> tag joinListSample
-- Product {getProduct = 6}
--
-- >>> let joinListSample = ((Append (Product 6) (Single (Product 2) 'e') (Single (Product 3) 'a'))) +++ (Empty) 
-- >>> tag joinListSample
-- Product {getProduct = 6}
--
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) first (Empty) = first
(+++) (Empty) second = second
(+++) first second = (Append . (uncurry mappend)) (tag first, tag second) first second
