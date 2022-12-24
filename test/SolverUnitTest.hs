module SolverUnitTest (
    solverTests
) where

import Data.Ratio ( (%) )
import Test.HUnit ( assertEqual, Test(..) )
import Calculator.Parser.Parse (Expr(..), BinaryOp(..))
import Calculator.Solver.Solver (eval)

solverTest1 :: Test
solverTest1 = TestCase (assertEqual "for eval on tokens for \"8.8 * (9/4) - (1/4^(8.5^7))\"" (eval (BinaryExpr Sub (BinaryExpr Mul (NumberExpr (44 % 5)) (BinaryExpr Div (NumberExpr (9 % 1)) (NumberExpr (4 % 1)))) (BinaryExpr Div (NumberExpr (1 % 1)) (BinaryExpr Pow (NumberExpr (4 % 1)) (BinaryExpr Pow (NumberExpr (17 % 2)) (NumberExpr (7 % 1)))))))(Right (17797162035136927486520121388811344862817972091528835070069578034615534904744595350138139254918346066090891274107267942408220187112627245626791895633307938313399021449061683042353919740523363317825855212256415571114976917551903825265917397943541896479171700557774043525754777347909491914178700276632798189584379 % 898846567431157953864652595394512366808988489471153286367150405788663379027504815663542386612037680105600569399356966788293948844072083112464237153197370621888839467124327426381511098006230470597265414760425028844190753411712314407369565552704136185816752553422931491199736229692398581524176781648121120686080)))

solverTest2 :: Test
solverTest2 = TestCase (assertEqual "for eval on tokens for \"9^5\"" (eval (BinaryExpr Pow (NumberExpr (9 % 1)) (NumberExpr (5 % 1))))(Right (59049 % 1)))

solverTest3 :: Test
solverTest3 = TestCase (assertEqual "for eval on tokens for \"9.5+89\"" (eval (BinaryExpr Add (NumberExpr (19 % 2)) (NumberExpr (89 % 1))))(Right (197 % 2)))

solverTest4 :: Test
solverTest4 = TestCase (assertEqual "for eval on tokens for \"18 / 9 * 17^4\"" (eval (BinaryExpr Mul (BinaryExpr Div (NumberExpr (18 % 1)) (NumberExpr (9 % 1))) (BinaryExpr Pow (NumberExpr (17 % 1)) (NumberExpr (4 % 1)))))(Right (167042 % 1)))

solverTest5 :: Test
solverTest5 = TestCase (assertEqual "for eval on tokens for \"9.8 - (17^(2)*7) + 19\"" (eval (BinaryExpr Add (BinaryExpr Sub (NumberExpr (49 % 5)) (BinaryExpr Mul (BinaryExpr Pow (NumberExpr (17 % 1)) (NumberExpr (2 % 1))) (NumberExpr (7 % 1)))) (NumberExpr (19 % 1)))) (Right ((-9971) % 5)))

solverTests :: Test
solverTests = TestList [
        TestLabel "testing for eval on tokens for \"8.8 * (9/4) - (1/4^(8.5^7))\"" solverTest1,
        TestLabel "testing for eval on tokens for \"9^5\"" solverTest2,
        TestLabel "testing for eval on tokens for \"9.5+89\"" solverTest3,
        TestLabel "testing for eval on tokens for \"18 / 9 * 17^4\"" solverTest4,
        TestLabel "testing for eval on tokens for \"9.8 - (17^(2)*7) + 19\"" solverTest5
    ]