module PolymorphismAndTypeClasses where

import ExprT
import Parser
import Control.Applicative

-- | Evaluates an expression
--
-- >>> eval (Mul (Add (Lit 2) (Lit 3)) (Lit 3))
-- 15
--
-- >>> eval (Add (Lit 3) (Lit 2))
-- 5
--
eval :: ExprT -> Integer
eval expression = case expression of
                  Lit x -> x
                  Add x y -> eval x + eval y
                  Mul x y -> eval x * eval y

-- | Evaluates arithmetic expressions given as strings
--
-- >>> evalStr "(2+3)*3"
-- Just 15
--
-- >>> evalStr "(3+3) + (2+2) * 10"
-- Just 46
--
-- >>> evalStr "wat"
-- Nothing
--
evalStr :: String -> Maybe Integer
evalStr str = eval <$> parseExp Lit Add Mul str

-- | Creating a type class Expr to abstract the properties of ExprT
--
-- >>> mul (add (lit 2) (lit 3)) (lit 4) :: ExprT
-- Mul (Add (Lit 2) (Lit 3)) (Lit 4)
--
class Expr a where
  add :: a -> a -> a
  mul :: a -> a -> a
  lit :: Integer -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul


-- | Instance of Expr for Integer
--
-- >>> (mul (add (lit 2) (lit 3)) (lit 3)) :: Integer
-- 15
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

-- | Instance of Expr for Bool
--
-- >>> (lit 3) :: Bool
-- True
--
-- >>> (lit (-2)) :: Bool
-- False
--
-- >>> (add (lit 1) (lit 1)) :: Bool
-- True
--
-- >>> (add (lit (-1)) (lit 1)) :: Bool
-- True
--
-- >>> (add (lit (-1)) (lit (-1))) :: Bool
-- False
--
-- >>> (mul (lit 1) (lit 1)) :: Bool
-- True
--
-- >>> (mul (lit (-1)) (lit 1)) :: Bool
-- False
--
-- >>> (mul (lit 1) (lit (-1))) :: Bool
-- False
--
-- >>> (mul (lit (-1)) (lit (-1))) :: Bool
-- False
--
-- >>> (mul (add (lit 2) (lit 3)) (lit 4)) :: Bool
-- True
--
instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)
