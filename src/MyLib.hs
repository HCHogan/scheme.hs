module MyLib (someFunc, readExpr, parseNumber, LispVal (..)) where

import Numeric (readBin, readDec, readHex, readOct)
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | Float Double -- Unused
  deriving (Show, Eq)

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many (noneOf "\"" <|> parseEscapedChar)
  _ <- char '"'
  return $ String x

parseEscapedChar :: Parser Char
parseEscapedChar = do
  _ <- char '\\'
  x <- oneOf $ fst <$> escapedChars
  case lookup x escapedChars of
    Just ch -> return ch
    Nothing -> fail "parseEscapedChar: invalid escaped character"
 where
  escapedChars = [('\\', '\\'), ('"', '"'), ('n', '\n'), ('r', '\r'), ('t', '\t')]

-- An `atom` is a letter or symbol, followed by any number of letters, digits, or symbols
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

-- A number may be written in binary, octal, decimal, or hexadecimal by the use of a radix prefix.
-- The radix prefixes are #b (binary), #o (octal), #d (decimal), and #x (hexadecimal).
-- With no radix prefix, a number is assumed to be expressed in decimal.
parseNumber :: Parser LispVal
parseNumber = parseNoRadixPrefix <|> parseRadixPrefix

-- TODO: Support full numeric tower of scheme

parseNoRadixPrefix :: Parser LispVal
parseNoRadixPrefix = Number . read <$> many1 digit

parseRadixPrefix :: Parser LispVal
parseRadixPrefix = do
  _ <- char '#'
  radix <- oneOf $ fst <$> radixReaders
  digits <- many1 hexDigit -- all the digits allowed in this syntax
  case lookup radix radixReaders of
    Just reader -> case reader digits of
      n : _ -> return $ Number $ fst n
      _ -> fail "parseRadixPrefix: invalid number"
    Nothing -> fail "parseRadixPrefix: invalid radix"
 where
  radixReaders = [('b', readBin), ('o', readOct), ('d', readDec), ('x', readHex)]

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  h <- endBy parseExpr spaces
  t <- char '.' >> spaces >> parseExpr
  return $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> do
      _ <- char '('
      x <- try parseList <|> parseDottedList -- try is used to backtrack if parseList fails
      _ <- char ')'
      return x

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Exercises --
_parseNumber' :: Parser LispVal
_parseNumber' = do
  num <- many1 digit
  return $ Number $ read num

_parseNumber'' :: Parser LispVal
_parseNumber'' = many1 digit >>= \x -> return $ Number $ read x
