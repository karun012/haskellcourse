module PolymorphismAndTypeClasses where

import ExprT

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
