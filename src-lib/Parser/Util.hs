module Parser.Util where

import Control.Applicative ((<|>))
import Parser.Types
import Text.Megaparsec (many, optional, try)
import Text.Megaparsec.Char (char, eol, letterChar, lowerChar, space, string, upperChar)
import Text.Megaparsec.Char.Lexer

assignParser :: Parser ()
assignParser = do
  _ <- char '=' *> sc
  return ()

delimiterParser :: IndentationLevel -> Parser ()
delimiterParser indentationLevel = do
  _ <- (eol *> indentationParser indentationLevel) <|> char ',' *> sc *> optional (try eol *> indentationParser indentationLevel) *> hole'
  return ()

hole' :: Parser ()
hole' = do
  return ()

uppercaseIdentifierParser :: Parser String
uppercaseIdentifierParser = do
  firstChar <- upperChar
  rest <- many (letterChar <|> char '\'')

  return (firstChar : rest)

lowercaseIdentifierParser :: Parser String
lowercaseIdentifierParser = do
  firstChar <- lowerChar
  rest <- many (letterChar <|> char '\'')

  return (firstChar : rest)

indentationParser :: IndentationLevel -> Parser ()
indentationParser indentationLevel = do
  _ <- string (replicate indentationLevel '\t')
  return ()

sc :: Parser ()
sc = do
  _ <- many (char ' ')
  return ()