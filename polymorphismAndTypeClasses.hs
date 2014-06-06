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
