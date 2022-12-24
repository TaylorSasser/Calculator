module LexerUnitTest (
    lexerTests
) where

import Data.Ratio ( (%) )
import Test.HUnit (assertEqual, Test(..))
import Calculator.Parser.Lex (isNumSymbol, isSpace, asRational, lexer, Token(..))

isNumSymbolTest1 :: Test
isNumSymbolTest1 = TestCase (assertEqual "for isNumSymbol 'a'," (isNumSymbol 'a') False)

isNumSymbolTest2 :: Test
isNumSymbolTest2 = TestCase (assertEqual "for isNumSymbol '.'," (isNumSymbol '.') True)

isNumSymbolTest4 :: Test
isNumSymbolTest4 = TestCase (assertEqual "for isNumSymbol '9'," (isNumSymbol '9') True)

isSpaceTest :: Test
isSpaceTest = TestCase (assertEqual "for isSpace ' '," (isSpace ' ') True)

asRationalTest1 :: Test
asRationalTest1 = TestCase (assertEqual "for asRational \"9\"," (asRational "9") (9 :: Rational))

asRationalTest2 :: Test
asRationalTest2 = TestCase (assertEqual "for asRational \"9.5\"," (asRational "9.5") (19 % 2 :: Rational))

lexerTest1 :: Test
lexerTest1 = TestCase (assertEqual "for lexer \"9+5\"," (lexer "9+5") (Right [TokenNumber (9 % 1),TokenAdd,TokenNumber (5 % 1)]))

lexerTest3 :: Test
lexerTest3 = TestCase (assertEqual "for lexer \"29*17*7\"," (lexer "29*17*7") (Right [TokenNumber (29 % 1), TokenMul,TokenNumber (17 % 1),TokenMul,TokenNumber (7 % 1)]))

lexerTest4 :: Test
lexerTest4 = TestCase (assertEqual "for lexer \"8/4\"," (lexer "8/4") (Right [TokenNumber (8 % 1),TokenDiv,TokenNumber (4 % 1)]))

lexerTest5 :: Test
lexerTest5 = TestCase (assertEqual "for lexer \"9^5\"," (lexer "9^5") (Right [TokenNumber (9 % 1),TokenPow,TokenNumber (5 % 1)]))

lexerTest6 :: Test
lexerTest6 = TestCase (assertEqual "for lexer \"9/0\"," (lexer "9/0") (Right [TokenNumber (9 % 1),TokenDiv,TokenNumber (0 % 1)]))

lexerTest7 :: Test
lexerTest7 = TestCase (assertEqual "for lexer \"9/1\"," (lexer "9/1") (Right [TokenNumber (9 % 1), TokenDiv, TokenNumber(1 % 1)]))


lexerTests :: Test
lexerTests = TestList [
        TestLabel "testing isNumSymbol 'a'" isNumSymbolTest1,
        TestLabel "testing isNumSymbol '.'" isNumSymbolTest2,
        TestLabel "testing isNumSymbol '9'" isNumSymbolTest4,

        TestLabel "testing isSpace ' '" isSpaceTest,

        TestLabel "testing asRational \"9\"" asRationalTest1,
        TestLabel "testing asRational \"9.5\"" asRationalTest2,

        TestLabel "testing for lexer \"9+5\"" lexerTest1,
        TestLabel "testing for lexer \"29*17*7\"" lexerTest3,
        TestLabel "testing for lexer \"8/4\"" lexerTest4,
        TestLabel "testing for lexer \"9^5\"" lexerTest5,
        TestLabel "testing for lexer \"9/0\"" lexerTest6,
        TestLabel "testing for lexer \"9/1\"" lexerTest7
    ]
