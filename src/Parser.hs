module Parser
  ( symbol,
    parseString,
    parseAtom,
    parseExpr,
    parse,
    LispVal (Atom, List, DottedList, VChar, VInt, VFloat, VString, VBool),
  )
where

import Control.Monad
import Numeric (readFloat)
import Text.ParserCombinators.Parsec

-- Lisp AST.
data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | VChar Char
  | VInt Integer
  | VFloat Float
  | VString String
  | VBool Bool
  deriving (Ord, Eq)

-- Parsing functions.
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ VString x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> VBool True
    "#f" -> VBool False
    _ -> Atom atom

parseInteger :: Parser LispVal
parseInteger = liftM (VInt . read) $ many1 digit

parseFloat :: Parser LispVal
parseFloat =
  liftM (VFloat . read) $
    (many1 digit >> char '.' >> many1 digit)

parseNumber :: Parser LispVal
parseNumber = try parseFloat <|> parseInteger

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> do
      char '('
      x <- try parseList <|> parseDottedList
      char ')'
      return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail
