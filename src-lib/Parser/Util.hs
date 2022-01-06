module Parser.Util where

import Control.Applicative ((<|>))
import Parser.Types
import Text.Megaparsec (many, optional, unPos)
import Text.Megaparsec.Char (char, eol, letterChar, lowerChar, space, string, upperChar)
import Text.Megaparsec.Char.Lexer

assignParser :: Parser ()
assignParser = do
  _ <- char '=' *> sc
  return ()

delimiterParser :: Parser ()
delimiterParser = do
  _ <- char ',' *> sc
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

indentationParser :: Parser a -> Parser [a]
indentationParser parser = do
  isEndOfLine <- optional eol
  indentationLevel <- indentLevel
  case isEndOfLine of
    Just _ -> do
      hasIndentation <- optional (string (replicate (unPos indentationLevel) '\t'))
      case hasIndentation of
        Just _ -> do
          result <- parser
          nextResults <- indentationParser parser
          return (result : nextResults)
        Nothing ->
          do return []
    Nothing -> do
      result <- parser
      return [result]

sc :: Parser ()
sc = do
  _ <- many (char ' ')
  return ()