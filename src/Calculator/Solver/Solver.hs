{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Calculator.Solver.Solver (
    eval
) where

import Calculator.Parser.Parse (Expr(..), BinaryOp (..))
newtype SolveError = SolveError String deriving (Show, Eq)

handleop :: BinaryOp -> Rational -> Rational -> Rational
handleop Add = (+)
handleop Sub = (-)
handleop Mul = (*)
handleop Div = (/)
handleop Pow = pow

pow :: Rational -> Rational -> Rational
pow x n = toRational (fromRational x ** fromRational n)

solver :: Expr -> Rational
solver (NumberExpr n) = n
solver (BinaryExpr op x y) = handleop op a b
    where
        a = solver x
        b = solver y

eval :: Expr -> Either SolveError Rational
eval x = Right (solver x)