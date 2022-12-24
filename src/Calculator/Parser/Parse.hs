{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module Calculator.Parser.Parse (
    Expr(..),
    BinaryOp(..),
    parse,
) where

import Calculator.Parser.Lex (Token (..))
import Calculator.Parser.Parser (Parser, (<|>), pluck, chainl1, one, runParser)
import Control.Applicative (optional)

newtype ParseError = ParseError String deriving (Show, Eq)

data Expr
    = BinaryExpr BinaryOp Expr Expr
    | VariableExpr String
    | NumberExpr Rational
    | FunctionExpr Function [Expr]
    deriving (Show, Eq)

data Function
    = Log
    | Ln
    | Sin
    | Cos
    deriving (Show, Eq)

data BinaryOp
    = Add
    | Sub
    | Mul
    | Div
    | Pow
    deriving (Show, Eq)

number :: Parser [Token] Expr
number = NumberExpr <$> pluck (\case
    TokenNumber n -> Just n
    _ -> Nothing)


-- function :: Parser [Token] Expr
-- function = do
--     FunctionExpr <$> pluck (\case
--         (TokenFunction f) -> Just n
--         _ -> Nothing)


expr :: Parser [Token] Expr
expr = term `chainl1` addop
term :: Parser [Token] Expr
term = power `chainl1` mulop
power :: Parser [Token] Expr
power = factor `chainl1` powop

factor :: Parser [Token] Expr
factor = (f <$> optional (one TokenSub)) <*> (number <|> one TokenRParen *> expr <* one TokenLParen)
    where
        f Nothing x = x
        f _ (NumberExpr v) = NumberExpr (negate v)


powop :: Parser [Token] (Expr -> Expr -> Expr)
powop = BinaryExpr Pow <$ one TokenPow

mulop :: Parser [Token] (Expr -> Expr -> Expr)
mulop = (BinaryExpr Mul <$ optional (one TokenMul)) <|> (BinaryExpr Div <$ one TokenDiv)

addop :: Parser [Token] (Expr -> Expr -> Expr)
addop = (BinaryExpr Add <$ one TokenAdd) <|> (BinaryExpr Sub <$ one TokenSub)

parse :: [Token] -> Either ParseError Expr
parse tokens = case runParser expr tokens of
    Nothing -> Left (ParseError "Unable to parse token stream")
    Just (x, _) -> Right x