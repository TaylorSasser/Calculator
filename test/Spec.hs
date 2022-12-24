{-# OPTIONS_GHC -Wno-unused-do-bind #-}
import Test.HUnit ( Counts )
import Test.HUnit.Text (runTestTT)
import LexerUnitTest (lexerTests)
import ParserUnitTest (parserTests)
import SolverUnitTest (solverTests)

main :: IO Counts
main = do
    runTestTT lexerTests
    runTestTT parserTests
    runTestTT solverTests



