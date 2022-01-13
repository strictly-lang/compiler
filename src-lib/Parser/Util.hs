module Parser.Util where

import Control.Applicative ((<|>))
import Control.Monad.State.Strict (put)
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
  _ <- try (eol *> indentationParser indentationLevel) <|> char ',' *> sc *> optional (try eol *> indentationParser indentationLevel) *> hole'
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
  _ <- put indentationLevel
  return ()

sc :: Parser ()
sc = do
  _ <- many (char ' ')
  return ()