{-# LANGUAGE LambdaCase #-}
module Calculator.Parser.Parser (
    Alternative (empty, (<|>), many, some),
    MonadPlus, 
    mfilter,
    guard,
    Parser,
    runParser,
    anything,
    satisfy,
    pluck,
    one,
    seq,
    sepBy,
    sepBy1,
    chainl1,
    chainl,
    chainr1,
    chainr,
    ignore,
    oneOf,
) where

import Control.Applicative (Alternative (empty, (<|>), many, some))
import Control.Monad ( MonadPlus, mfilter, guard )
import Control.Monad.State (StateT (runStateT, StateT))
import Data.List (foldl1')

import Prelude hiding (seq)

newtype Parser s a = Parser {
    unwrap :: StateT s Maybe a
} deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

runParser :: Parser s a -> s -> Maybe (a, s)
runParser = runStateT . unwrap

anything :: Parser [s] s
anything = Parser . StateT $ \case
    []     -> empty
    (c:cs) -> pure (c, cs)

pluck :: (s -> Maybe a) -> Parser [s] a
pluck f =
  Parser . StateT $ \case
    t : ts -> case f t of
      Just res -> pure (res, ts)
      _ -> Nothing
    _ -> Nothing

satisfy :: (s -> Bool) -> Parser [s] s
satisfy predicate = do
    c <- anything
    guard $ predicate c
    pure c

one :: Eq s => s -> Parser [s] s
one c = mfilter (== c) anything

seq :: Eq s => [s] -> Parser [s] [s]
seq =  traverse one

sepBy :: Parser [s] a -> Parser [s] b -> Parser [s] [a]
sepBy p sep = (p `sepBy1` sep) <|> pure []

sepBy1 :: Parser [s] a -> Parser [s] b -> Parser [s] [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

chainl :: Parser [s] a -> Parser [s] (a -> a -> a) -> a -> Parser [s] a
chainl p op a = (p `chainl1` op) <|> pure a

chainl1 :: Parser [s] a -> Parser [s] (a -> a -> a) -> Parser [s] a
chainl1 p op = p >>= rest
    where
        rest a = (do
            f <- op
            b <- p
            rest (f a b)) <|> pure a

chainr :: Parser [s] a -> Parser [s] (a -> a -> a) -> a -> Parser [s] a
chainr p op a = (p `chainr1` op) <|> pure a

chainr1 :: Parser [s] a -> Parser [s] (a -> a -> a) -> Parser [s] a
chainr1 p op = scan
    where
        scan = p >>= rest
        rest a = (do
            f <- op
            b <- scan
            rest (f a b)) <|> pure a

ignore :: (s -> Bool) -> Parser [s] [s]
ignore f = many (satisfy f)

oneOf :: Alternative f => [f a] -> f a
oneOf = foldl1' (<|>)