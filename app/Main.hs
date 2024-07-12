module Main where

import MyLib
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ readExpr $ head args
