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

delimiterParser :: Parser ()
delimiterParser = do
  _ <- (eol *> hole') <|> char ',' *> sc *> optional eol *> hole'
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

indentationParser :: IndentationLevel -> Parser a -> Parser a
indentationParser indentationLevel parser = do
  indentationParser' indentationLevel parser <|> parser

indentationParser' :: IndentationLevel -> Parser a -> Parser a
indentationParser' indentationLevel parser = do
  result <- try (optional eol *> string (replicate indentationLevel '\t') *> parser)
  put indentationLevel
  return result

sc :: Parser ()
sc = do
  _ <- many (char ' ')
  return ()