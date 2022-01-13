module Parser.Util where

import Control.Applicative ((<|>))
import Control.Monad.State.Strict (get, put)
import Parser.Types
import Text.Megaparsec (between, many, optional, sepBy, try)
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

blockParser :: Parser begin -> Parser end -> Parser a -> Parser [a]
blockParser beginParser endParser contentParser = do
  indentationLevel <- get
  between (beginParser *> sc *> optional (try (eol *> indentationParser (indentationLevel + 1)))) endParser (contentParser `sepBy` delimiterParser (indentationLevel + 1))

indentationParser :: IndentationLevel -> Parser ()
indentationParser indentationLevel = do
  _ <- string (replicate indentationLevel '\t')
  _ <- put indentationLevel
  return ()

sc :: Parser ()
sc = do
  _ <- many (char ' ')
  return ()