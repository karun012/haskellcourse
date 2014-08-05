{-# LANGUAGE DataKinds #-}
module FoldsAndMonoids where

import JoinList
import Data.Monoid
import Sized

-- | Gets the annotation at the root of a JoinList
--
-- >>> tag (Single (Product 5) 'y')
-- Product {getProduct = 5}
--
-- >>> tag (Append (Product 6) (Single (Product 2) 'e') (Single (Product 3) 'a'))
-- Product {getProduct = 6}
--
-- >>> tag (Append (Product 6) (Single (Product 2) 'e') (Single (Product 3) 'a'))
-- Product {getProduct = 6}
--
tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m
tag (Empty) = mempty

-- | Joins two JoinLists
--
-- >>> let joinListSample = (Single (Product 5) 'y') +++ (Empty)
-- >>> tag joinListSample
-- Product {getProduct = 5}
--
-- >>> let joinListSample = (Empty) +++ (Single (Product 6) 'y')
-- >>> tag joinListSample
-- Product {getProduct = 6}
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
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) first (Empty) = first
(+++) (Empty) second = second
(+++) first second = (Append . (uncurry mappend)) (tag first, tag second) first second

-- | Finds the JoinList element at the specified index
--
-- >>> indexJ 10 (Single (Size 3) 'y')
-- Nothing
--
-- >>> indexJ 1 (Single (Size 1) 'y')
-- Just 'y'
--
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ index joinList
                | index > getSize' joinList = Nothing
                | index == getSize' joinList = getValue joinList

getSize' :: (Sized b, Monoid b) => JoinList b a -> Int
getSize' = (getSize . size . tag)

getValue :: JoinList b a -> Maybe a
getValue (Single _ a) = Just a
getValue (Empty) = Nothing
