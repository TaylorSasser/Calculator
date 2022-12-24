{-# LANGUAGE BlockArguments #-}
module ParserUnitTest (
    parserTests
) where

import Data.Ratio ( (%) )
import Test.HUnit ( assertEqual, Test(..) )
import Calculator.Parser.Parse (parse, Expr(..), BinaryOp(..))
import Calculator.Parser.Lex (Token(..))

parseTest1 :: Test
parseTest1 = TestCase (assertEqual "for parse \"9^5\"" (parse [TokenNumber (9 :: Rational ),TokenPow,TokenNumber (5 :: Rational)])(Right (BinaryExpr Pow (NumberExpr (9 % 1)) (NumberExpr (5 % 1)))))

parseTest2 :: Test
parseTest2 = TestCase (assertEqual "for parse \"9.5+89\"" (parse [TokenNumber (19 % 2),TokenAdd,TokenNumber (89 % 1)])(Right (BinaryExpr Add (NumberExpr (19 % 2)) (NumberExpr (89 % 1)))))

parseTest3 :: Test
parseTest3 = TestCase (assertEqual "for parse \"18 / 9 * 17^4\"" (parse [TokenNumber (18 % 1),TokenDiv,TokenNumber (9 % 1),TokenMul,TokenNumber (17 % 1),TokenPow,TokenNumber (4 % 1)])(Right (BinaryExpr Mul (BinaryExpr Div (NumberExpr (18 % 1)) (NumberExpr (9 % 1))) (BinaryExpr Pow (NumberExpr (17 % 1)) (NumberExpr (4 % 1))))))

parseTest4 :: Test
parseTest4 = TestCase (assertEqual "for parse \"9.8 - (17^(2)*7) + 19\"" (parse [TokenNumber (49 % 5),TokenSub,TokenRParen,TokenNumber (17 % 1),TokenPow,TokenRParen,TokenNumber (2 % 1),TokenLParen,TokenMul,TokenNumber (7 % 1),TokenLParen,TokenAdd,TokenNumber (19 % 1)])(Right (BinaryExpr Add (BinaryExpr Sub (NumberExpr (49 % 5)) (BinaryExpr Mul (BinaryExpr Pow (NumberExpr (17 % 1)) (NumberExpr (2 % 1))) (NumberExpr (7 % 1)))) (NumberExpr (19 % 1)))))

parseTest5 :: Test
parseTest5 = TestCase (assertEqual "for parse \"8.8 * (9/4) - (1/4^(8.5^7))\"" (parse [TokenNumber (44 % 5),TokenMul,TokenRParen,TokenNumber (9 % 1),TokenDiv,TokenNumber (4 % 1),TokenLParen,TokenSub,TokenRParen,TokenNumber (1 % 1),TokenDiv,TokenNumber (4 % 1),TokenPow,TokenRParen,TokenNumber (17 % 2),TokenPow,TokenNumber (7 % 1),TokenLParen,TokenLParen])(Right (BinaryExpr Sub (BinaryExpr Mul (NumberExpr (44 % 5)) (BinaryExpr Div (NumberExpr (9 % 1)) (NumberExpr (4 % 1)))) (BinaryExpr Div (NumberExpr (1 % 1)) (BinaryExpr Pow (NumberExpr (4 % 1)) (BinaryExpr Pow (NumberExpr (17 % 2)) (NumberExpr (7 % 1))))))))


parserTests :: Test
parserTests = TestList [
        TestLabel "testing for parse \"9^5\"" parseTest1,
        TestLabel "for parse \"9.5+89\"" parseTest2,
        TestLabel "for parse \"18 / 9 * 17^4\"" parseTest3,
        TestLabel "for parse \"9.8 - (17^(2)*7) + 19\"" parseTest4,
        TestLabel "for parse \"8.8 * (9/4) - (1/4^(8.5^7))\"" parseTest4
    ]