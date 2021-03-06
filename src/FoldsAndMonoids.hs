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
-- >>> indexJ 0 (Single (Size 0) 'y')
-- Just 'y'
--
-- >>> let left = Append (Size 1) (Single (Size 1) 'y') Empty
-- >>> let right = Append (Size 2) (Single (Size 1) 'z') (Single (Size 1) 'a')
-- >>> let root = left +++ right
-- >>> indexJ 0 root
-- Just 'y'
--
-- >>> indexJ 1 root
-- Just 'z'
--
-- >>> indexJ 2 root
-- Just 'a'
--
-- >>> Just (jlToList root !! 1) == indexJ 1 root
-- True
--

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2


indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ (Empty)              = Nothing
indexJ 0 (Single m a)         = Just a
indexJ n (Single m a) | n > 0 = Nothing
indexJ n _            | n < 0 = Nothing
indexJ n (Append m jl1 jl2)
  | n < leftSize = indexJ n jl1
  | otherwise = indexJ (n - leftSize) jl2
  where leftSize = getSize . size $ (tag jl1)
