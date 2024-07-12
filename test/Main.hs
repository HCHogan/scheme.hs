module Main (main) where

import MyLib (LispVal (..), parseNumber)
import Test.HUnit
import Text.ParserCombinators.Parsec hiding (parseTest, spaces)

parseNumberTest :: String -> Either ParseError LispVal
parseNumberTest = parse parseNumber ""

tests :: Test
tests =
  TestList
    [ TestCase (assertEqual "Binary test" (Right (Number 11)) (parseNumberTest "#b1011"))
    , TestCase (assertEqual "Octal test" (Right (Number 83)) (parseNumberTest "#o123"))
    , TestCase (assertEqual "Decimal test" (Right (Number 123)) (parseNumberTest "#d123"))
    , TestCase (assertEqual "Hexadecimal test" (Right (Number 419)) (parseNumberTest "#x1A3"))
    , TestCase (assertBool "Invalid prefix test" (isLeft (parseNumberTest "#z123")))
    ]

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

main :: IO ()
main = do
  _ <- runTestTT tests
  return ()
