module Main where

import REPL (eval, readExpr)
import System.Environment

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
