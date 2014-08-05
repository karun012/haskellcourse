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
-- >>> let emptyList = Empty
-- >>> let singleElementListA = Single (Product 6) 'y'
-- >>> let singleElementListB = Single (Product 3) 'y'
-- >>> let twoElementList = Append (Product 6) (Single (Product 2) 'e') (Single (Product 3) 'a')
--
-- >>> let singlePlusEmpty = singleElementListA +++ emptyList
-- >>> tag singlePlusEmpty
-- Product {getProduct = 6}
--
-- >>> let emptyPlusSingle = emptyList +++ singleElementListA
-- >>> tag emptyPlusSingle
-- Product {getProduct = 6}
--
-- >>> let singlePlusSingle = singleElementListA +++ singleElementListB
-- >>> tag singlePlusSingle
-- Product {getProduct = 18}
--
-- >>> let twoElementPlusSingle = twoElementList +++ singleElementListA
-- >>> tag twoElementPlusSingle
-- Product {getProduct = 36}
--
-- >>> let singlePlusTwoElement = singleElementListA +++ twoElementList
-- >>> tag singlePlusTwoElement
-- Product {getProduct = 36}
--
-- >>> let twoElementPlusTwoElement = twoElementList +++ twoElementList
-- >>> tag twoElementPlusTwoElement
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
