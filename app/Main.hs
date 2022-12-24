module Main (main) where

import System.IO (hSetEncoding, utf8, stdout)
import Calculator.App (start)

main :: IO ()
main = hSetEncoding stdout utf8 >> start