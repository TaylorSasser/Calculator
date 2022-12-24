module Calculator.Parser.Lex (
    lexer,
    isSpace,
    asRational,
    isNumSymbol,
    Token(..),
) where

import Calculator.Parser.Parser (Parser, (<|>), ignore, oneOf, some, satisfy, seq, runParser)
import Data.Text.Read ( rational )
import Data.Text (pack)
import Data.Char (isDigit, isAlpha)
import Prelude hiding (seq)


data Token
    = Token
    | TokenNumber Rational
    | TokenFunction String
    | TokenVar Char
    | TokenRParen
    | TokenLParen
    | TokenAdd
    | TokenSub
    | TokenMul
    | TokenDiv
    | TokenPow
    | TokenRoot
    | TokenLog
    | TokenLn
    deriving(Eq, Show)

data LexerError
    = Unexpected String
    | Other
    deriving (Eq, Show)

isNumSymbol :: Char -> Bool
isNumSymbol '.' = True
isNumSymbol c = isDigit c

isSpace :: Char -> Bool
isSpace ' ' = True
isSpace _ = False


token :: Parser String a -> Parser String a
token p = p <* ignore isSpace

symbol :: String -> Parser String String
symbol = token . seq

asRational :: String -> Rational
asRational x = case rational (pack x) of
    Right (a, _) -> a
    _ -> 0

constant :: Parser String Token
constant = oneOf
    [
        TokenVar '𝜋' <$ symbol "pi",
        TokenVar '𝜋' <$ symbol "𝜋",
        TokenVar '𝒆' <$ symbol "e",
        TokenVar '𝒆' <$ symbol "𝒆"
    ]


function :: Parser String Token
function = oneOf
    [
        TokenFunction "ln"  <$ symbol "ln",
        TokenFunction "log" <$ symbol "log",
        TokenFunction "sin" <$ symbol "sin",
        TokenFunction "cos" <$ symbol "cos",
        TokenFunction "tan" <$ symbol "tan",
        TokenFunction "sec" <$ symbol "sec",
        TokenFunction "csc" <$ symbol "csc",
        TokenFunction "cot" <$ symbol "cot"
    ]

variable :: Parser String Token
variable = TokenVar <$> token (satisfy isAlpha)


operator :: Parser String Token
operator = oneOf
    [
        TokenRParen <$ symbol "(",
        TokenLParen <$ symbol ")",
        TokenAdd <$ symbol "+",
        TokenSub <$ symbol "-",
        TokenMul <$ symbol "*",
        TokenDiv <$ symbol "/",
        TokenPow <$ symbol "^"
    ]

number :: Parser String Token
number =  (TokenNumber . asRational <$> some (satisfy isNumSymbol)) <* ignore isSpace

grammar :: Parser String Token
grammar = number <|> operator <|> constant <|> function

lexer :: String -> Either LexerError [Token]
lexer chars = case runParser (some grammar) chars of
    Nothing -> Left (Unexpected "")
    Just (x, _) -> Right x