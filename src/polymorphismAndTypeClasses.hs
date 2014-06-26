{-# LANGUAGE FlexibleInstances #-}

module PolymorphismAndTypeClasses where

import ExprT
import Parser
import Control.Applicative
import Data.Maybe
import qualified Data.Map as M

-- | Evaluates an expression
--
-- >>> eval (Mul (Add (Lit 2) (Lit 3)) (Lit 3))
-- 15
--
-- >>> eval (Add (Lit 3) (Lit 2))
-- 5
--
eval :: ExprT -> Integer
eval (Lit x)   = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

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


-- | Instance of Expr for MinMax
--
-- >>> (lit 4) :: MinMax
-- MinMax 4
--
-- >>> (add (lit 4) (lit 8)) :: MinMax
-- MinMax 8
--
-- >>> (mul (lit 4) (lit 8)) :: MinMax
-- MinMax 4
--
newtype MinMax = MinMax Integer deriving (Show, Eq)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

-- | Instance of Expr for Mod7
--
--
-- >>> (lit 4) :: Mod7
-- Mod7 4
--
-- >>> (lit 8) :: Mod7
-- Mod7 1
--
-- >>> (lit (-2)) :: Mod7
-- Mod7 5
--
-- >>> (add (lit 6) (lit 9)) :: Mod7
-- Mod7 1
--
-- >>> (mul (lit 4) (lit 10)) :: Mod7
-- Mod7 5
--
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7 . flip mod 7
  add (Mod7 x) (Mod7 y) = Mod7 (flip mod 7 $ x + y)
  mul (Mod7 x) (Mod7 y) = Mod7 (flip mod 7 $ x * y)


class HasVars a where
    var :: String -> a

data VarExprT = VarLit Integer
           | VarAdd VarExprT VarExprT
           | VarMul VarExprT VarExprT
           | Variable String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = VarLit
  add = VarAdd
  mul = VarMul

instance HasVars VarExprT where
  var = Variable

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

maybeAdd :: Maybe Integer -> Maybe Integer -> Maybe Integer
maybeAdd (Just x) (Just y) = Just (x + y)
maybeAdd _ _ = Nothing

maybeMultiply :: Maybe Integer -> Maybe Integer -> Maybe Integer
maybeMultiply (Just x) (Just y) = Just (x * y)
maybeMultiply _ _ = Nothing

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit fn = (<$>) Just (\_ -> fn)
  add x y = (<$>) maybeAdd x <*> y
  mul x y = (<$>) maybeMultiply x <*> y

-- | Testing calculator with intermediate vars
--
-- >>> withVars [("x", 4)] $ (var "x")
-- Just 4
--
-- >>> withVars [("x", 6)] $ add (lit 3) (var "x")
-- Just 9
--
-- >>> withVars [("x", 6)] $ add (lit 3) (var "y")
-- Nothing
--
-- >>> withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))
-- Just 54
--
withVars :: [(String, Integer)]
    -> (M.Map String Integer -> Maybe Integer)
    -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

